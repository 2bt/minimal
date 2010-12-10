"std/std.gs" require

8		: ticks_per_row ;
750		frame

3		channel
80		volume

{
	[
		{	30		sustain
			30		decay
			5		release
			play
		  3 wave C_6 pitch }
		{ 0 wave C_3 pitch }
		{ 3 wave C_5 pitch }
		{ 0 wave G_2 pitch }
		{ 0 wave C_2 pitch }
		{ 0 wave G_1 pitch }
		{ 0 wave F_1 pitch }
		{ 0 wave E_1 pitch }
		{ 0 wave G_1 pitch stop }
		{ 3 wave C_5 pitch }
	] m_do
} : beat ;

{
	[
		{	30		sustain
			40		decay
			19		release
			play
		  3 wave C_7 pitch }
		{ }
		{ 0 wave G_3 pitch }
		{ 0 wave F_3 pitch }
		{ 0 wave E_3 pitch }
		{ 0 wave D_3 pitch }
		{ 3 wave C_8 pitch }
		{ stop }
	] m_do
} : snare ;

{
	[
		{	0		sustain
			10		decay
			play
		  3 wave C_9 pitch }
	] m_do
} : hat ;

[
	{
		{ beat }
		{ hat }
		{ }
		{ hat }
		{ snare }
		{ }
		{ hat }
		{ beat }
		{ hat }
		{ beat }
		{ hat }
		{ }
		{ snare }
		{ }
		{ }
		{ hat }
			} 4 *
	;; { beat } { beat }
] : beat1 ;


2		channel
50		volume
3		wave
60		decay
0		sustain

[	{ play 40 panning [C_4 C_4 C_5 C_5] m_arpeggio }
	{ { } } 15 *
	{ play -40 panning [C_2 C_2 C_3 C_3] m_arpeggio }
	{ { } } 15 *

] {{}}% : percussion1 ;


0		channel
1		wave
0		sustain
30		decay
-10		panning
140		volume
[
	{
		{ play 10 pulse [ C_2 G_2 C_3 G_3 ] m_arpeggio }
		{ }
		{ }
		{ }
			} 3 *

	{ play [ C_2 G_2 C_3 G_3 ] m_arpeggio }
	{ }
	{ play 3 pulse [ G_1 D_2 G_2 D_3 ] m_arpeggio }
	{ }

	{
		{ play 10 pulse [ C_2 G_2 C_3 G_3 ] m_arpeggio }
		{ }
		{ }
		{ }
			} 4 *
] {{}}% : sub_bass1 ;



1		channel
1		wave
0		pulse
20		decay
0		sustain
10		panning
80		volume

[
	{
		{ 30,{15-..}%.{~)}%+{2*{panning}+}%m_loop	# wtf
		  play C_4 pitch } { }
		{ play D_4 pitch } { }
		{ play Ds4 pitch } { }
		{ play G_4 pitch } { }
		{ play As4 pitch } { }
		{ play G_4 pitch } { }
		{ play Ds4 pitch } { }
		{ play D_4 pitch } { }
			} 3 *
	{ play C_4 pitch } { }
	{ play D_4 pitch } { }
	{ play Ds4 pitch } { }
	{ play G_4 pitch } { }
	{ play As4 pitch } { }
	{ play G_4 pitch } { }
	{ play Ds4 pitch } { }
	{ play } { play D_4 pitch }
] : pad1 ;


4		channel
0		wave
10		release
[
	{
		{ }
		{ play C_2 pitch }
		{ stop }
		{ play C_2 pitch }
		{ stop }
		{ }
		{ }
		{ }
			} 8 *
		;;; { play As2 pitch } { stop } { }
] : bass1 ;

[{}]128*:_;

[
	[ sub_bass1	_		_			_		_		]
	[ sub_bass1	_		_			beat1	_		]
	[ sub_bass1	_		percussion1	beat1	bass1	] .
	[ sub_bass1	pad1	percussion1	beat1	bass1	] .
	[ sub_bass1	pad1	_			_		bass1	] .
	[ sub_bass1 [{}]8* ] [ [{}]24* ]
] : patterns ;





