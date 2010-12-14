"std/std.gs" require

"title: c64-memories
original author: smt (1994)"puts

12		: ticks_per_row ;
480		frame

####### bass #######
{
	120	volume
	20	pulse

	30	sustain
	15	decay
	-2	panning

} : init_bass ;

[
	{ play C_3 pitch } { init_bass } +
	{}
	{ [C_3 0 10] m_pitch_slide }
	{}
] : bass0 ;

[
	{
		{ play As2 pitch 100 volume m_none } { init_bass } +
		{ play C_3 pitch }
		{ play As2 pitch 50 volume }
		{ play C_2 pitch 100 volume }
		{ play C_3 pitch }
		{ play C_2 pitch }
		{ play C_3 pitch 50 volume }
		{ play C_3 pitch 100 volume }
		{ play C_2 pitch }
		{ play C_3 pitch 50 volume }
		{ play C_3 pitch 100 volume }
		{ stop }
		{ play As1 pitch }
		{ play C_2 pitch }
		{ stop }
		{ play C_3 pitch }
	} 2 *
	;
	{ stop }

	{ play As2 pitch 100 volume }
	{ play C_3 pitch }
	{ play As2 pitch 50 volume }
	{ play C_2 pitch 100 volume }
	{ play C_3 pitch }
	{ play C_2 pitch }
	{ play C_3 pitch 50 volume }
	{ play C_3 pitch 100 volume }
	{ play C_2 pitch }
	{ play C_3 pitch 50 volume }
	{ play C_3 pitch 100 volume }
	{ stop }
	{ play As1 pitch }
	{ play C_2 pitch }
	{ stop }
	{ play C_3 pitch }

	{ play As2 pitch 100 volume }
	{ play C_3 pitch }
	{ play As2 pitch 50 volume }
	{ play C_2 pitch 100 volume }
	{ play C_3 pitch }
	{ play C_2 pitch }
	{ play C_3 pitch 50 volume }
	{ play C_3 pitch 100 volume }
	{ play Ds1 pitch }
	{ play C_3 pitch 50 volume }
	{ play Ds2 pitch 100 volume }
	{ stop }
	{ play F_2 pitch }
	{ play F_2 pitch }
	{ stop }
	{ stop }
] : bass1 ;

[
	{
		{ play Gs2 pitch 100 volume m_none } { init_bass } +
		{ play As2 pitch }
		{ play Gs2 pitch 50 volume }
		{ play As1 pitch 100 volume }
		{ play As2 pitch }
		{ play As1 pitch }
		{ play As2 pitch 50 volume }
		{ play As2 pitch 100 volume }
		{ play As1 pitch }
		{ play As2 pitch 50 volume }
		{ play As2 pitch 100 volume }
		{ stop }
		{ play Gs1 pitch }
		{ play As1 pitch }
		{ stop }
		{ play As2 pitch }
	} 2 *
	;
	{ stop }

	{ play Gs2 pitch 100 volume }
	{ play As2 pitch }
	{ play Gs2 pitch 50 volume }
	{ play As1 pitch 100 volume }
	{ play As2 pitch }
	{ play As1 pitch }
	{ play As2 pitch 50 volume }
	{ play As2 pitch 100 volume }
	{ play As1 pitch }
	{ play As2 pitch 50 volume }
	{ play As2 pitch 100 volume }
	{ stop }
	{ play Gs1 pitch }
	{ play As1 pitch }
	{ stop }
	{ play As2 pitch }

	{ play Gs2 pitch 100 volume }
	{ play As2 pitch }
	{ play Gs2 pitch 50 volume }
	{ play As1 pitch 100 volume }
	{ play As2 pitch }
	{ play As1 pitch }
	{ play As2 pitch 50 volume }
	{ play As2 pitch 100 volume }
	{ play Cs1 pitch }
	{ play As2 pitch 50 volume }
	{ play Cs2 pitch 100 volume }
	{ stop }
	{ play Ds2 pitch }
	{ play Ds2 pitch }
	{ stop }
	{ stop }
] : bass2 ;

[
	bass2 ~
	{;} 16 *
	{ play Gs2 pitch 100 volume }
	{ play As2 pitch }
	{}
	{ play Gs2 pitch 32 volume }
	{ play As2 pitch }
	{}
	{ play Gs2 pitch 16 volume }
	{ play As2 pitch }
	{}
	{ play Gs2 pitch 8 volume }
	{ play As2 pitch }
	{}
	{}
	{}
	{}
	{ stop }
] : bass3 ;

####### chord #######
{
	0	wave
	30	pulse
	60	volume
	25	release
	10	panning

} : init_chord ;

[
	{
		{} { init_chord } +
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 Gs5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 D_5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 Gs5] m_arpeggio }
		{ stop }
		{}
		{ play [C_5 Ds5 G_5] m_arpeggio }
		{ stop }
	} 2 *
	;;
	{ play [C_5 Ds5 As5] m_arpeggio }
	{ stop }

] : chord1 ;

[
	{
		{} { init_chord } +
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 Fs5] m_arpeggio }
		{ stop }
		{}
		{ play [Gs4 C_5 Ds5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 Fs5] m_arpeggio }
		{ stop }
		{}
		{ play [As4 Cs5 F_5] m_arpeggio }
		{ stop }
	} 2 *
	;;
	{ play [As4 Cs5 Gs5] m_arpeggio }
	{ stop }

] : chord2 ;

[
	chord2 ~
	{;} 16 *
	{{}} 16 *
] : chord3 ;

####### lead #######
{
	100	pulse
	1	wave

	80	sustain
	-10 panning
	40	release

} : init_lead ;

[
	{ play [D_5 Ds5 1] m_pitch_slide 100 volume } { init_lead } +
	{}
	{ D_5 pitch m_none 30 volume }
	{ play D_5 pitch 100 volume }
	{ [D_5 10 100] m_vibrato }
	{ [D_5 0 4] m_pitch_slide }
	{ play C_5 pitch m_none }
	{ D_5 pitch 30 volume }
	{}
	{ play Ds5 pitch 100 volume }
	{ [Ds5 10 100] m_vibrato }
	{ [Ds5 0 2] m_pitch_slide }
	{ play D_5 pitch m_none }
	{ Ds5 pitch 30 volume }
	{ play G_4 pitch 100 volume}
	{ D_5 pitch 30 volume }

	{ play Ds5 pitch 100 volume }
	{ [Ds5 0 2] m_pitch_slide }
	{ play D_5 pitch m_none }
	{ play C_5 pitch }
	{ D_5 pitch 30 volume }
	{}
	{ play G_4 pitch 100 volume }
	{ [G_4 10 100] m_vibrato }
	{ [G_4 0 3] m_pitch_slide }
	{ play D_5 pitch m_none }
	{ G_4 pitch 30 volume }
	{}
	{ play Ds5 pitch 100 volume }
	{ [Ds5 F_5 2] m_pitch_slide }
	{ play F_5 pitch m_none }
	{ Ds5 pitch 30 volume }

	{ play Ds5 pitch 100 volume }
	{ play D_5 pitch }
	{ play C_5 pitch }
	{ play Ds5 pitch }
	{ play D_5 pitch }
	{ play G_4 pitch }
	{ play Ds5 pitch }
	{ play D_5 pitch }
	{ play C_5 pitch }
	{ play Ds5 pitch }
	{ play D_5 pitch }
	{ play G_4 pitch }
	{ play Ds5 pitch }
	{ play D_5 pitch }
	{ play Ds5 pitch }
	{ play F_5 pitch }
	{ play [F_5 G_5 3] m_pitch_slide }
	{}
	{ F_5 pitch 30 volume m_none }
	{ play [F_5 G_5 3] m_pitch_slide 100 volume }
	{}
	{ F_5 pitch 30 volume m_none }
	{ play [F_5 G_5 3] m_pitch_slide 100 volume }
	{}
	{ F_5 pitch 30 volume m_none }
	{ play [F_5 G_5 3] m_pitch_slide 100 volume }
	{}
	{ F_5 pitch 30 volume m_none }
	{ play Ds5 pitch 100 volume }
	{ G_5 pitch 30 volume }
	{ play D_5 pitch 100 volume }
	{ Ds5 pitch 30 volume }
] : lead1 ;

[
	{ play [C_5 Cs5 1] m_pitch_slide 100 volume } { init_lead } +
	{}
	{ C_5 pitch m_none 30 volume }
	{ play C_5 pitch 100 volume }
	{ [C_5 10 100] m_vibrato }
	{ [C_5 F_5 8] m_pitch_slide }
	{ play F_5 pitch m_none }
	{ C_5 pitch 30 volume }
	{}
	{ play Fs5 pitch 100 volume }
	{ [Fs5 10 100] m_vibrato }
	{ [Fs5 0 2] m_pitch_slide }
	{ play F_5 pitch m_none }
	{ Fs5 pitch 30 volume }
	{ play Ds5 pitch 100 volume}
	{ F_5 pitch 30 volume }

	{ play F_5 pitch 100 volume }
	{ [F_5 Fs5 2] m_pitch_slide }
	{ play Fs5 pitch m_none }
	{ play F_5 pitch }
	{ Fs5 pitch 30 volume }
	{}
	{ play Cs5 pitch 100 volume }
	{ [Cs5 10 100] m_vibrato }
	{ [Cs5 0 3] m_pitch_slide }
	{ play C_5 pitch m_none }
	{ Cs5 pitch 30 volume }
	{}
	{ play Cs5 pitch 100 volume }
	{ [Cs5 Ds5 2] m_pitch_slide }
	{ play Ds5 pitch m_none }
	{ Cs5 pitch 30 volume }

	{ play F_5 pitch 100 volume }
	{ play Cs5 pitch }
	{ play C_5 pitch }
	{ play F_5 pitch }
	{ play Cs5 pitch }
	{ play As4 pitch }
	{ play F_5 pitch }
	{ play Cs5 pitch }
	{ play C_5 pitch }
	{ play F_5 pitch }
	{ play Cs5 pitch }
	{ play As4 pitch }
	{ play F_5 pitch }
	{ play Cs5 pitch }
	{ play C_5 pitch }
	{ play As4 pitch }
	{ play [C_5 Cs5 1] m_pitch_slide }
	{}
	{ C_5 pitch 30 volume m_none }
	{ play [C_5 Cs5 1] m_pitch_slide 100 volume }
	{}
	{ C_5 pitch 30 volume m_none }
	{ play [C_5 Cs5 1] m_pitch_slide 100 volume }
	{}
	{ C_5 pitch 30 volume m_none }
	{ play [C_5 Cs5 1] m_pitch_slide 100 volume }
	{}
	{ C_5 pitch 30 volume m_none }
	{ play C_5 pitch 100 volume }
	{ Cs5 pitch 30 volume }
	{ play As4 pitch 100 volume }
	{ C_5 pitch 30 volume }
] : lead2 ;

[
	lead2 ~
	{;} 8 *
	{ [Cs5 As4 10] m_pitch_slide }
	{ [As4 10 100] m_vibrato }
	{}
	{}
	{ stop }
	{}
	{}
	{}
] : lead3 ;

####### echo #######
{
	init_lead
	15 panning
} : init_echo ;

{
	{
	`"100 volume"/"50 volume"*~
	`"30 volume"/"15 volume"*~
	`"init_lead"/"init_echo"*~
	} % [{} {}] \ +
} : make_echo ;

lead1 make_echo : echo1 ;
lead2 make_echo : echo2 ;
lead3 make_echo : echo3 ;

####### drum #######
{
	[
		{	30		sustain
			30		decay
			5		release
			4		panning
			play
		  3 wave G_5 pitch }
		{ 3 wave C_5 pitch }
		{ 0 wave C_3 pitch }
		{ 0 wave G_2 pitch }
		{ 0 wave E_2 pitch }
		{ 0 wave C_2 pitch }
		{ 0 wave B_1 pitch }
		{ 0 wave A_1 pitch }
		{ 0 wave G_1 pitch stop }
		{ 3 wave C_6 pitch }
	] m_do 100 volume
} : beat ;

{
	[
		{	30		sustain
			40		decay
			19		release
			-8		panning
			play
		  3 wave C_7 pitch }
		{}
		{ 0 wave G_3 pitch }
		{ 0 wave F_3 pitch }
		{ 0 wave E_3 pitch }
		{ 0 wave D_3 pitch }
		{ 3 wave C_8 pitch }
		{ stop }
	] m_do 100 volume
} : snare ;


[
	{ snare }
	{ beat }
	{ snare }
	{ snare
		patterns(;:patterns;		# evil hack
		pattern_count(:pattern_count;
	}
] : drum0 ;

[
	{
		{
			{ beat }
			{ beat }
			{}
			{}
			{ snare }
			{}
			{ beat }
			{ beat }
			{}
			{ beat }
			{}
			{ beat }
			{ snare }
			{}
			{ beat }
			{}
		} 2 *
		;
		{ snare }
	} 2 *
	{;} 7 *
	{ snare }
	{}
	{ beat }
	{ snare }
	{}
	{ snare }
	{ snare }
] : drum1 ;

[
	drum1 ~
	{;} 16 *
	{ beat }
	{}
	{}
	{ beat 50 volume }
	{}
	{}
	{ beat 25 volume }
	{}
	{}
	{ beat 12 volume }
	{}
	{}
	{ snare }
	{}
	{ snare }
	{ snare }
] : drum2 ;

####### patterns #######

[
	[ bass0 drum0 _ _ _ ]

	[ bass1 drum1 chord1 _ _ ]
	[ bass2 drum1 chord2 _ _ ]
	[ bass1 drum1 chord1 _ _ ]
	[ bass3 drum2 chord3 _ _ ]

	[ bass1 drum1 chord1 lead1 echo1 ]
	[ bass2 drum1 chord2 lead2 echo2 ]
	[ bass1 drum1 chord1 lead1 echo1 ]
	[ bass3 drum2 chord3 lead3 echo3 ]

] : patterns ;
