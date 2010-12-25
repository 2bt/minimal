"std/std.gs" require

32		: ticks_per_row ;
100		frame

2		attack
80		decay
0		sustain
10		release
0		wave
40		pulse

{[.120-\10]m_pitch_slide} : m_funny ;

[
	{ C_4 m_funny play }
	{}
	{ E_4 m_funny play }
	{}
	{ G_4 m_funny play }
	{}
	{ D_4 m_funny play }
	{}
	{ Fs4 m_funny play }
	{}
	{ A_4 m_funny play }
	{}
	{ E_4 m_funny play }
	{}
	{ Gs4 m_funny play }
	{}
	{ B_4 m_funny play }
	{ [B_4 6 30] m_vibrato }
	{}
	{}
	{ [B_4 0 5] m_pitch_slide }
	{ stop }

	{{}} 10 *
	{ m_none }

] : melody ;


[{0:pw;[]{0 channel	pw sin 30/40+pulse pw 3+:pw;}macro}{{}}200*]:qqq;

[ [ melody qqq ] ] : patterns ;
