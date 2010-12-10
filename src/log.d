module src.log;

import std.stream;
import std.stdio;

short swap(short x) {
	return ((x & 0xff) << 8) + (x >> 8);
}

uint swap(uint x) {
	return	((x & 0xff) << 24) + ((x & 0xff00) << 8) +
			((x >> 8) & 0xff00) + (x >> 24);
}

uint swap_uint(uint x) { return swap(x); }
short swap_short(short x) { return swap(x); }

class SoundLog {

	Stream	file;
	uint	frames = 0;

	this(string filename) {
		file = new BufferedFile(filename, FileMode.OutNew);

		file.writeBlock("FORM".ptr, 4);
		file.write(0);				// total size
		file.writeBlock("AIFF".ptr, 4);

		file.writeBlock("COMM".ptr, 4);
		file.write(swap_uint(18));
		file.write(swap_short(2));	// stereo
		file.write(0);				// number of frames
		file.write(swap_short(16));	// 16 bit
		// write 48000 in FPU double extended precision format
		foreach(ubyte s; [0x40, 0x0e, 0xbb, 0x80, 0, 0, 0, 0, 0, 0])
			file.write(s);

		file.writeBlock("SSND".ptr, 4);
		file.write(0);				// data size + 8
		file.write(0);
		file.write(0);
	}
	void write(short left, short right) {
		frames++;
		file.write(swap(left));
		file.write(swap(right));
	}

	~this() {
		uint size =	frames * 2 * 16 / 8;	// 16 bit stereo
		uint sound_size = size + 8;
		uint total_size = 4 + 8 + 18 + 8 + size + 8;
		file.seekSet(4);
		file.write(swap(total_size));
		file.seekSet(22);
		file.write(swap(frames));
		file.seekSet(42);
		file.write(swap(sound_size));
		file.close;
		delete file;
	}
}


