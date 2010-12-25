module src.main;

import std.stdio;
import std.math;
import std.file;
import std.path;
import std.gc;


import src.golf;
import src.golfobject;
import src.log;

extern (C) {
	int		SDL_Init(uint flags);
	void	SDL_Quit();
	struct SDL_AudioSpec {
		int		freq;
		ushort	format;
		ubyte	channels;
		ubyte	silence;
		ushort	samples;
		ushort	padding;
		uint	size;
		void function (void* userdata, ubyte* stream, int len) callback;
		void*	userdata;
	}
	int SDL_OpenAudio(SDL_AudioSpec* desired, SDL_AudioSpec* obtained);
	void SDL_PauseAudio(int pause_on);
}

class	Channel {
	enum { RELEASE, ATTACK, HOLD }
	enum { PULSE, TRIANGLE, SINE, NOISE }

	float	phase		= 0;
	uint	shift_reg	= 0x7ffff8;
	float	speed		= 0;
	float	volume;
	float	pan_left;
	float	pan_right;

	float	attack;
	float	decay;
	float	sustain;
	float	release;
	int		state;
	float	level = 0;

	int		wave;
	float	pulsewidth;
}

class Synth {
	enum { MIXRATE = 48000, BUFFERLENGTH = 1024 }

	string			path;
	SoundLog		log;

	Golf			golf;
	Channel[int]	channels;
	Channel			chan;
	int				chan_index;

	int				sample = 0;
	int				frame_size;

	const string default_settings = "
		10000	frame
		0		channel
	";
	const string default_channelsettings = "
		0		state
		100		volume
		0		panning
		2		attack
		30		decay
		50		sustain
		2		release
		0		wave
		50		pulse
	";

	this(string filename, bool logging=false) {
		golf = new Golf;
		golf.userdata = cast(void*) this;

		golf.table["require"] = Golf.Value((Golf golf) {
			string filename = std.path.join(path, golf.pop.getString);
			string code = cast(string) read(filename);
			golf.exec(code);
		});
		golf.table["channel"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.chan_index = golf.pop.getNumber;
			if(!(s.chan_index in s.channels)) {
				s.chan = s.channels[s.chan_index] = new Channel;
				golf.exec(s.default_channelsettings);
			}
			else s.chan = s.channels[s.chan_index];
		});
		golf.table["frame"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.frame_size = golf.pop.getNumber;
		});
		golf.table["wave"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.chan.wave = golf.pop.getNumber;
		});
		golf.table["state"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			int state = golf.pop.getNumber;
			s.chan.state = state;
		});
		golf.table["volume"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.chan.volume = golf.pop.getNumber / 100.0;
		});
		golf.table["panning"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			float pan = golf.pop.getNumber / 100.0 + 0.5;
			s.chan.pan_left = sqrt(1 - pan);
			s.chan.pan_right = sqrt(pan);
		});
		golf.table["pitch"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			int pitch = golf.pop.getNumber;
			s.chan.speed = (440.0 / MIXRATE) *
				pow(2, cast(float)(pitch - (9 + 12 * 4) * 16) * (1.0 / (12 * 16)));
		});
		golf.table["attack"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			float f = golf.pop.getNumber * (2.5 / 100);
			s.chan.attack = 1 / (f * f * MIXRATE + 0.0001);
		});
		golf.table["sustain"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.chan.sustain = golf.pop.getNumber / 100.0;
		});
		golf.table["decay"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			float f = golf.pop.getNumber * (1.5 / 100);
			s.chan.decay = 1 - 1.0 / (f * f * MIXRATE + 1);
		});
		golf.table["release"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			float f = golf.pop.getNumber * (1.5 / 100);
			s.chan.release = 1 - 1.0 / (f * f * MIXRATE + 1);
		});
		golf.table["pulse"] = Golf.Value((Golf golf) {
			auto s = cast(Synth) golf.userdata;
			s.chan.pulsewidth = golf.pop.getNumber / 100.0;
		});


		writefln("loading...");
		golf.exec(default_settings);

		path = std.path.getDirName(filename);
		string code = cast(string) read(filename);
		golf.exec(code);

		if(logging) {
			string l = std.path.getBaseName(std.path.getName(filename)) ~ ".aiff";
			log = new SoundLog(l);
		}

		auto spec = SDL_AudioSpec(MIXRATE, 0x8010, 2, 0,
			BUFFERLENGTH, 0, 0, &fill_buffer, cast(void*) this);
		SDL_Init(16);
		SDL_OpenAudio(&spec, null);
		writefln("playing...");
		SDL_PauseAudio(0);
	}

	~this() {
		SDL_Quit();
		golf.exec("\"stack: \"print]`(;);puts");
		delete golf;
		delete log;
	}

	void mix(out float left, out float right) {
		if(!sample) {
			try {
				golf.exec("tick");
			}
			catch (Exception e) {
				writefln("Error: %s", e);
				//SDL_PauseAudio(1);
				throw e;
			}
		}
		if(++sample == frame_size) sample = 0;
		left	= 0;
		right	= 0;
		foreach(ref c; channels) {

			// adsr
			switch (c.state) {
			case Channel.RELEASE:
				c.level *= c.release;
				break;
			case Channel.ATTACK:
				c.level += c.attack;
				if(c.level > 1) {
					c.level = 1;
					c.state = Channel.HOLD;
				}
				break;
			case Channel.HOLD:
			default:
				c.level = c.sustain + (c.level - c.sustain) * c.decay;
				break;
			}

			// osc
			c.phase += c.speed;
			if(c.wave != Channel.NOISE)
				c.phase -= cast(int) c.phase;

			float amp;
			switch(c.wave) {
			case Channel.PULSE:
			default:
				amp = c.phase < c.pulsewidth ? -1 : 1;
				break;
			case Channel.TRIANGLE:
				amp = (c.phase < c.pulsewidth) ? 
					(2.0 / c.pulsewidth) * c.phase - 1 :
					(-2.0 / (1 - c.pulsewidth)) * (c.phase - c.pulsewidth) + 1;
				break;
			case Channel.SINE:
				amp = sin(c.phase * 2 * PI);
				break;
			case Channel.NOISE:
				uint s = c.shift_reg;
				uint b;
				while(c.phase > 0.1) {
					c.phase -= 0.1;
					b = ((s >> 22) ^ (s >> 17)) & 1;
					s = ((s << 1) & 0x7fffff) + b;
				}
				c.shift_reg = s;
				amp = (
					((s & 0x400000) >> 11) |
					((s & 0x100000) >> 10) |
					((s & 0x010000) >> 7) |
					((s & 0x002000) >> 5) |
					((s & 0x000800) >> 4) |
					((s & 0x000080) >> 1) |
					((s & 0x000010) << 1) |
					((s & 0x000004) << 2)) * (1.0 / (1<<12)) - 0.5;
				break;
			}
//			amp = (cast(int) ((amp+1)*32))/32.0-1;
			
			amp *= c.volume * c.level;
			left += amp * c.pan_left;
			right += amp * c.pan_right;
		}
	}

	static extern (C) void fill_buffer(void* dummy, ubyte* stream, int len) {
		short *buffer = cast(short*)stream;
		auto synth = cast(Synth) dummy;
		float l, r;
		for(int m = 0; m < len >> 2; m++, buffer += 2) {
			synth.mix(l, r);
			buffer[0] = cast(short)(l * 7000);
			buffer[1] = cast(short)(r * 7000);
			// logging
			if(synth.log)
				synth.log.write(buffer[0], buffer[1]);
		}
	}
}


void main(string[] args) {
	disable;//TODO: fix this!!!!

	string filename;
	bool logging = false;

	foreach(a; args[1 .. $]) {
		if(a == "-l") logging = true;
		else filename = a;
	}

	if(!filename) {
		writefln("usage: %s [-l] filename", args[0]);
		return;
	}

	auto synth = new Synth(filename, logging);
	readln;
	delete synth;
}


