# helper
{{[\].~>=}*}:min;
{{[\].~<=}*}:max;
{1 state}:play;
{0 state}:stop;

# generate note names
[ "C_CsD_DsE_F_FsG_GsA_AsB_" 2 / 12 * 120 , ] zip { . 1 = 16 * \ ~ 12 / + { } \ + ~ ; } /

####### player #######
4 : ticks_per_row ;
[] : macros : macro_states ;
0 : pattern_count : row_count : tick_count ;
{

	tick_count !
	{
		# initialize macros
		patterns { , } % max .
		macros , > {
			. [ {} ] * : macros ;
			. [ [] ] * : macro_states ;
		} * ;

		patterns pattern_count = .
		. , , [ \ [macro_states macros] zip \ { row_count = } % ] zip
		{ ( channel ~ ~ } %
		
		. { 0 = } % : macro_states ;
		{ 1 = } % : macros ;

		{ , } % min row_count ) . @ =
		{ pattern_count ) patterns , % : pattern_count ; ; 0 } * : row_count ;

	} *

	tick_count ) ticks_per_row % : tick_count ;

	# execute macros
	[ macros . , , \ macro_states \ ] zip
	{ ( channel ~ ~ } % : macro_states ;

} : tick ;



####### macros #######
{[\\]\;} : macro ;

{[]{}macro}									: m_none ;

{{.,!!{(~}*}macro}							: m_do ;

{{(.~`"[]"1/\*~+}macro}						: m_loop ;

{
	.,ticks_per_row\/("."*{}+%{(.pitch+}
	macro
}						: m_arpeggio ;		# [ pitch ... ]


{	{
		[~.@+\]
		.~;<{[~;;.0]}*
		.1=pitch
	} macro
}						: m_pitch_up ; 		# [ target_pitch source_pitch speed ]


{	{
		[~.@+\]
		.~;>{[~;;.0]}*
		.1=pitch
	} macro
}						: m_pitch_down ;	# [ target_pitch source_pitch speed ]


{	0 + {
		)\.2=@++...3=sin\1=*1000/\0=+pitch
	} macro
}						: m_vibrato ;		# [ pitch amplitude speed ]



