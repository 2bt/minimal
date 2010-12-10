"std/std.gs" require

# player
0 : count ;
{	melody count = ~				# execute row
	count ) melody , % : count ;	# inc count
} : tick ;

5500	frame
100		volume
2		attack
18		decay
0		sustain
8		release
0		wave

[
  {
	{ play C_2 pitch 30 pulse 15 panning }
	{ stop }
	{ play C_3 pitch -5 panning }
	{ play C_3 pitch 60 pulse }
	{ play C_2 pitch 50 pulse 15 panning }
	{ stop }
	{ play As2 pitch 80 pulse }
	{ play C_3 pitch 70 pulse -15 panning }
	{ play C_2 pitch 50 pulse 15 panning }
	{ stop }
	{ play G_3 pitch -15 panning }
	{ play C_3 pitch }
	{ play Cs2 pitch 30 pulse 15 panning }
	{ play C_2 pitch 60 pulse -15 panning }
	{ play C_3 pitch 50 pulse 15 panning }
	{ play Cs3 pitch 10 pulse -15 panning }
  } 3 *

	{ play G_2 pitch 30 pulse 15 panning }
	{ stop }
	{ play G_3 pitch -5 panning }
	{ play G_3 pitch 60 pulse }
	{ play F_2 pitch 50 pulse 15 panning }
	{ stop }
	{ play F_3 pitch 80 pulse }
	{ play F_3 pitch 70 pulse -15 panning }
	{ play Ds2 pitch 50 pulse 15 panning }
	{ stop }
	{ play Ds3 pitch -15 panning }
	{ play Ds3 pitch }
	{ play Cs2 pitch 30 pulse 15 panning }
	{ play C_2 pitch 60 pulse -15 panning }
	{ play C_3 pitch 50 pulse 15 panning }
	{ play Cs3 pitch 10 pulse -15 panning }

] : melody ;

