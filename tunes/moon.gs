"std/std.gs" require

8			: ticks_per_row ;
400			frame

####### lead #######
{
	2	wave
	30	pulse
	30	attack
	70	release
	20	panning

} : lead_init ;

[
	{ play [ Ds4 6 100 ] m_vibrato } { lead_init } +
	{}
	{}
	{}
	{}
	{}
	{}
	{}
	{ stop [~\(; C_4 \+\] }
	{}
	{}
	{}
	{}
	{}
	{}
	{}
	{}
	{{}}16*

] {{}}% : lead1 ;

[
	{ play [ G_4 6 100 ] m_vibrato } { lead_init -20 panning} +
	{}
	{}
	{}
	{}
	{}
	{}
	{}
	{ stop }
	{}
	{}
	{}
	{ [~\(; As4 \+\] }
	{}
	{}
	{}
	{}
	{{}}4*
	{ [~\(; A_4 \+\] }
	{{}}11*

] {{}}% : lead2 ;

####### bass #######
{
	0	wave
	10	pulse
	3	attack
	5	release
	-5	panning
	70	volume

} : bass_init ;

[
	{ play C_2 pitch m_none } { bass_init } +
	{}
	{ stop }
	{}
	{ play [ C_3 As2 2 ] m_pitch_up }
	{}
	{}
	{}
	{ stop }
	{}
	{ play Ds3 pitch m_none }
	{ stop }
	{ play C_2 pitch }
	{ stop }
	{ play [ Ds2 C_2 3 ] m_pitch_up }
	{}
	{}
	{ stop }
	{}
	{}
	{ play C_2 pitch m_none }
	{}
	{ play G_1 pitch }
	{}
	{ play As1 pitch }
	{}
	{ play C_2 pitch }
	{}
	{}
	{ stop }
	{ play C_2 pitch }
	{}
	{ stop }
	{{}} 16 *
	{ play G_1 pitch }
	{ stop }
	{ play G_1 pitch }
	{}
	{ play G_2 pitch }
	{ stop }
	{ play As1 pitch }
	{}
	{ play As2 pitch }
	{ stop }
	{ play B_1 pitch }
	{}
	{ play B_2 pitch }
	{ stop }
] : bass1 ;


####### patterns #######
[
	[ lead1 lead2 bass1 ]

] : patterns ;



