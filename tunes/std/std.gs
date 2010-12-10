# helper
{{[\].~>=}*}:min;
{{[\].~<=}*}:max;
{1 state}:play;
{0 state}:stop;

# generate note names
[ "C_CsD_DsE_F_FsG_GsA_AsB_" 2 / 12 * 120 , ] zip { . 1 = 16 * \ ~ 12 / + { : } \ + ~ ; } /

# new player
4 : ticks_per_row ;
[] : macros : macro_states ;
0 : pattern_count : row_count : tick_count ;
{
	# initialize macros
	macros !
	{	patterns { , } % max .
		[ { } ] * : macros ;
		[ [ ] ] * : macro_states ;
	} *

	tick_count !
	{
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

# macros
{[\\]\;}									: macro ;
{{.,!!{(~}*}macro}							: m_do ;
{{(.~`"[]"1/\*~+}macro}						: m_loop ;
{.,ticks_per_row\/("."*{}+%{(.pitch+}macro}	: m_arpeggio ;

