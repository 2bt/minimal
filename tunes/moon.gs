"std/std.gs" require

8			: ticks_per_row ;
400			frame

####### lead #######
{
	3	wave
	30	pulse
#	30	attack
	70	release
	20	panning
	0	volume

} : lead_init ;

[
	{ play [ Ds3 300 100 ] m_vibrato } { lead_init } +
	{}
	{}
	{}
	{}
	{}
	{}
	{}

#	{ stop [~\(; C_4 \+\] }
	{ stop }

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
	{ play [ G_4 6 100 ] m_vibrato } { lead_init -20 panning 0 volume } +
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
	1	wave
	40	pulse
	3	attack
	5	release
	-5	panning

} : bass_init ;

[
	{ play C_2 pitch m_none } { bass_init } +
	{}
	{ stop }
	{}
	{ play [ As2 C_3 2 ] m_pitch_slide }
	{}
	{}
	{}
	{ stop }
	{}
	{ play Ds3 pitch m_none }
	{ stop }
	{ play C_2 pitch }
	{ stop }
	{ play [ C_2 Ds2 3 ] m_pitch_slide }
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
	{ stop }
	{}
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



