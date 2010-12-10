"std.gs" require

# player
0 : count ;
{	melody count = ~				# execute row
	count ) melody , % : count ;	# inc count
} : tick ;

# win sound
{
	2500	frame
	0		attack
	80		decay
	0		sustain
	15		release
	0		wave

	[
		{ C_4 pitch play 50 pulse 2500 frame }
		{ }
		{ E_4 pitch 40 pulse }
		{ }
		{ G_4 pitch 30 pulse }
		{ }
		{ D_4 pitch play 40 pulse }
		{ }
		{ Fs4 pitch 30 pulse }
		{ }
		{ A_4 pitch 20 pulse }
		{ }
		{ E_4 pitch play 30 pulse }
		{ }
		{ Gs4 pitch 20 pulse }
		{ }
		{ B_4 pitch 10 pulse }
	  	{ 100 frame B_4:ps; }
		{ { ps (( : ps pitch stop } } 300 *
	] : melody ;
}~

# phaser sound
{
	100		frame
	1		wave
	0		pulse
	[
	  {
		{ { ps . pitch 10 - : ps ; } } : q ;
		{ play C_8 : ps ; } q +
		{ q } 100 *
	  } 3 *
		{ stop }
		{ { } } 500 *
	] : melody ;
};

