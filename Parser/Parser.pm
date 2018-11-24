package PiPE::Parser;

use 5.010;
use Marpa::R2;
use strict;
no warnings 'experimental';



open my $gram_file, "<<", "/../Grammar/source.gram" or die "Could not find file default grammar: $!";

my $grammar = Marpa::R2::Scanless::G->new(
  {
    default_action => '[value]',
    source => \$gram_file
  }
);

my $parser = Marpa::R2::Scanless::R->new(
  {
    grammar => $grammar,
    semantics_package => 'PiPE::Actions',
    trace_values => 1,
  }
);

my $input;
open my $fh, "<", "test.txt";
$input .= $_ for <$fh>;

#say "\nINPUT: $input\n\n";

our %Vars;
our %Subs;
our %Types = ( scalar => '$', array => '@', hash => '%' );
our %Scopes = ( global => 'our', local => 'local' );

$parser->read( \$input );

my $value_ref = $parser->value;
my $value = ${$value_ref};
carp 'PiPE could not recognize your code.' unless $value;


use Data::Dumper;
say "\nSYMBOL_LIST: \n* ", join "\n* " => PiPE::Actions::extract( $value );
say "\n\n";



package PiPE::Actions;



#returns an array of leaf node values
#
#   recursively traverses an array of nodes until all leaf-nodes are extracted
#
sub extract {
  return map {
    my $node = shift;
    ref $node ? map( extract( $_ ) => @$node ) : $node;
  } @_;
}


sub extract_name {
  my $class = shift;
  my ($name) = extract @_;

  my $noun = $PiPE::Parser::Vars{ "\L$name" } // $name;

  $PiPE::Parser::Vars{ "\L$noun" } = "\L$noun" unless $PiPE::Parser::Vars;

  return $noun;
}


sub extract_noun {
  my $class = shift;
  my ($article, $name) = extract @_;

  my $noun = $PiPE::Parser::Vars{ "\L$name" } // $name;

  $PiPE::Parser::Vars{ "\L$noun" } = "\L$noun" unless $PiPE::Parser::Vars;

  return "\L$article" eq 'the' ? "\\$noun" : $noun;
}


sub extract_list {
  my $class = shift;
  my ($name) = extract @_;

  $name = $PiPE::Parser::Vars{ "\L$name" } // die "NO SUCH LIST AS '$name'!";

  die "'$name' IS NOT A LIST!" unless $name =~ /@/;

  return "$name";
}


sub say_paragraph {
  my $class = shift;
  my (@paragraph) = extract @_;

  return sprintf "%s;\n" x @paragraph => @paragraph;
}

sub say_about {
  my $class = shift;
  my ($title) = extract @_;
  
  return "package $title;\n";
}


sub say_language {
  my $class = shift;
  my ($language) = extract @_;
  
  given ( $language ) {
    $language = "use Languages::PiPE;" when /english/i;
    $language = "no Languages::PiPE;" when /perl/i;
    default { die "NO SUCH LANGUAGE FOUND: $language"; }
  }

  return "$language\n";
}


sub say_sentences {
  my $class = shift;
  my (@sentences) = extract @_;

  return join "\n" => @sentences;
}


sub say_block {
  my $class = shift;
  my ($declare, $block) = extract @_;
  
  my @lines = split "\n" => $block;
  $block = join ";\n\t" => @lines;

  return $declare ? "$declare {\n\t$block;\n}" : "{\n\t$block;\n}";
}


sub say_clause_chain {
  my $class = shift;
  my (@inputs) = extract @_;

  return join "\n" => @inputs;
}

sub say_clause_conditional {
  my $class = shift;
  my ($block, $context, $boolean) = extract @_;
  
  return "$block $context ( $boolean )";
}


sub say_questionless {
  my $class = shift;
  my ($left, $junction, $right) = extract @_;
  
  return "$left $junction $right";
}


sub say_question {
  my $class = shift;
  my ($condition, $true, $false) = extract @_;
  
  return "$condition ? $true : $false";
}


sub say_boolean {
  my $class = shift;
  my ($value, $boolean) = extract @_;
  
  my $output;
  given ( $boolean ) {
    $output = " ( $boolean $value )"  when m/   ~~ \Z/ix;
    $output = " ( $value $boolean )"  when m/\A eq /ix;
    $output = " ( $value $boolean )"  when m/\A =~ /ix;
    $output = " ( $value $boolean )"  when m/\A ~~ /ix;
    $output = "!( $value $boolean )"  when m/   \) \Z/ix;
  }
}


sub match_regex {
  my $class = shift;
  my ($name, $regex) = extract @_;
  
  #$name = $variables{ "$name" } // "my $name";

  return "$name =~ $regex"
}



sub say_conditional {
  my $class = shift;
  my ($context, $boolean) = extract @_;
  
  return "$context ( $boolean )"
}


sub say_do {
  my $class = shift;
  my ($value) = extract @_;
  
  return "";
}


sub say_sub {
  my $class = shift;
  my ($method) = extract @_;
  
  return "sub $method";
}


sub say_otherwise {
  my $class = shift;
  my ($context, $condition) = extract @_;
  
  return "else $context ( $condition )" if $condition;

  return "else";
}


sub say_label {
  my $class = shift;
  my ($label) = extract @_;
  
  return "$label:";
}


sub say_loop_block {
  my $class = shift;
  my ($context, $condition) = extract @_;
  
  return "$context $condition";
}


sub say_loop {
  my $class = shift;
  my ($list, $name) = reverse extract @_;
  
  return "( my \$$name = shift $list )" if $name;

  return "( shift $list )";
}


sub say_ref_loop {
  my $class = shift;
  my ($list, $name, $type) = reverse extract @_;
  
  return "(my \$$name = shift grep{ ref $_ eq '$type' } $list)" if $type;

  return "( shift grep{ ref $_ eq '$name' } $list )" if $name;
}


sub say_accessor {
  my $class = shift;
  my ($accessor, @list) = extract @_;
  
  my $output = pop @list;
  given ( $accessor ) {
    $output = "shift $output"  when /shift/i;
    $output = "pop $output"    when /pop/i;
    $output = "keys $output"   when /keys/i;
    $output = "values $output" when /values/i;
    $output = "$output".'[-1]' when /top/i;
    $output = "$output".'[00]' when /bottom/i;
  }

  return "( $output )"
}


sub say_typed_name {
  my $class = shift;
  my ($name) = extract @_;

  my $noun = $PiPE::Parser::Types{ "\L$name" } //
                $PiPE::Parser::Vars{ "\L$name" } //
                $name;

  return $noun;
}


sub say_scoped_name {
  my $class = shift;
  my ($scope, $name) = extract @_;

  my $noun = $PiPE::Parser::Types{ "\L$name" } //
                $PiPE::Parser::Vars{ "\L$name" } //
                $name;

  return "$scope $noun";
}


sub say_number {
  my $class = shift;
  my (@inputs) = extract @_;
  
  return join "" => @inputs;
}


sub say_decimal {
  my $class = shift;
  my ($lhs, $rhs) = extract @_;

  return "$lhs.$rhs";
}


sub say_exists {
  my $class = shift;
  my ($existence, $value) = extract @_;
  
  my $output;
  given( $existence ) {
    $output = "$value ~~"   when /has/i;
    $output = " eq $value"  when /equals/i;
    $output = " =~ $value"  when /matches/i;
    $output = " ~~ $value"  when /is/i;
    $output = " ~~ $value)" when /not/i;
  }

  return "$output";
}


sub say_value_chooser {
  my $class = shift;
  my ($state, $adjective, $value) = extract @_;
  
  my $output;
  if ( $state =~ /not/i ) {
    $output = ">= $value" if $adjective =~ /less/i;
    $output = "<= $value" if $adjective =~ /(more)|(greater)/i;
  } else {
    $output = "> $value" if $adjective =~ /(more)|(greater)/i;
    $output = "< $value" if $adjective =~ /less/i;
  }

  return "$output";
}


sub say_value_compare {
  my $class = shift;
  my ($state, $adjective, $value) = extract @_;
  
  my $output;
  if ( $state =~ /not/i ) {
    $output = "> $value" if $adjective =~ /less/i;
    $output = "< $value" if $adjective =~ /(more)|(greater)/i;
  } else {
    $output = ">= $value" if $adjective =~ /(more)|(greater)/i;
    $output = "<= $value" if $adjective =~ /less/i;
  }

  return "$output";
}


sub say_list_match {
  my $class = shift;
  my ($list) = extract @_;
  
  return "~~ $list";
}


sub declare_sub {
  my $class = shift;
  my ($name, $function) = extract @_;
  
  return "$name" . '->' . "$function";
}



sub declare_var {
  my $class = shift;
  my ($name, $type) = extract @_;

  $PiPE::Parser::Vars{ "$name" } = "$name";

  return "$type$name";
}


sub declare_filehandle {
  my $class = shift;
  my ($name, $type, $verb, $target) = extract @_;

  die "ONLY A SCALAR CAN BE A FILEHANDLE" unless $type =~ /\$/i;

  return "open( $type$name, '$verb', '$target' )";
}


sub say_pair {
  my $class = shift;
  my ($lhs, $rhs) = extract @_;

  return "$lhs => $rhs";
}


sub say_list_parse {
  my $class = shift;
  my ($name, $verb, $pattern) = extract @_;

  given ( $verb ) {
    $verb = 'map' when /mapped/;
    $verb = 'grep' when /selected/;
    $verb = 'sort' when /sorted/;
    default { die "NO VALID LIST PARSE PATTERN GIVEN" }
  }

  return "$verb { $pattern } $name";
}


sub do_binary_op {
  my $class = shift;
  my ($verb, $lhs, $prep, $rhs) = extract @_;

  return;
}


sub do_list_func {
  my $class = shift;
  my ($verb, $item, $target) = extract @_;

  return;
}


sub do_sort_list {
  my $class = shift;
  my ($target) = extract @_;
  return;
}


sub do_list_parse {
  my $class = shift;
  my ($verb, $list, $pattern) = extract @_;

  return;
}


sub do_output {
  my $class = shift;
  my ($verb, $value, $target) = extract @_;

  return;
}


sub do_unary_op {
  my $class = shift;
  my ($verb, $value) = extract @_;

  return;
}


sub do_list_access {
  my $class = shift;
  my ($verb, $target) = extract @_;

  return;
}


sub do_bitshift {
  my $class = shift;
  my ($lhs, $direction, $rhs) = extract @_;

  return;
}


sub do_seek {
  my $class = shift;
  my ($value, $direction, $name) = extract @_;

  return;
}


sub do_function {
  my $class = shift;
  my ($function) = extract @_;

  return;
}


sub say_last {
  my $class = shift;
  my ($target) = extract @_;

  return "last" . $target // "";
}


sub say_next {
  my $class = shift;
  my ($target) = extract @_;

  return "next" . $target // "";
}


sub say_flow {
  my $class = shift;
  my ($verb, $target) = extract @_;

  $verb = "return" if $verb eq 'stop';

  return "$verb" . $target // "";
}


sub say_goto {
  my $class = shift;
  my ($target) = extract @_;

  return "goto $target";
}


sub flatten_list {
  my $class = shift;
  my ($list) = extract @_;

  return "( $list )";
}


sub say_list {
  my $class = shift;
  my (@values) = extract @_;
  
  return join ", " => @values;
}


sub say_op_exp {
  my $class = shift;
  my ($lhs, $op, $rhs) = extract @_;

  return "($lhs $op $rhs)";
}


sub var_assign {
  my $class = shift;
  my ($name, $target) = extract @_;
  
  return "$name = $target";
}


sub post_declare_var {
  my $class = shift;
  my ($type, $name, $value) = extract @_;
  
  return "$type$name = $value";
}


sub post_assign_declare {
  my $class = shift;
  my ($type, $value, $name) = extract @_;

  return "$type$name = $value";
}


sub open_filehandle {
  my $class = shift;
  my ($target, $verb, $name, $scope, $article) = reverse extract @_;

  $scope = $PiPE::Parser::Scopes{ "$scope" } // 'my';

  $PiPE::Parser::Vars{ "$name" } = "$name";

  return "open( $scope $name, '$verb', '$target' )";
}


sub declare_list {
  my $class = shift;
  my ($name, $list) = extract @_;

  return "local \@$name = $list";
}


sub say_file_verb {
  my $class = shift;
  my $verb = join " " => extract @_;

  given ( $verb ) {
    $verb = '+<' when /reads and writes/i;   #open for reading and writing
    $verb = '<'  when /\Areads/i;            #open for reading
    $verb = '>'  when /\Awrites/i;           #open for clobbered writing
    $verb = '>>' when /\Aappends/i;          #open for appending
    $verb = '-|' when /\Apipes from/i;     #open incoming pipe
    $verb = '|-' when /\Apipes to/i;       #open outgoing pipe
    default { die "NO VALID FILE_VERB INDICATED" }
  }
  
  return $verb;
}

sub say_file_name {
  my $class = shift;
  my (@filename) = extract @_;
  
  return join "" => @filename;
}


sub say_function {
  my $class = shift;
  my ($function, @args) = extract @_;
  
  return @args ? "$function( @args )" : "$function()";
}


sub adjusted_exp {
  my $class = shift;
  my ($adjustment, $expression) = extract @_;
  
  my $output = $expression;

  given ($adjustment) {
    $output = "sort "
  }

  return $output;
}


sub say_regex {
  my $class = shift;
  my ($regex) = extract @_;
  
  return "m{$regex}"
}


sub say_quote {
  my $class = shift;
  my (@inputs) = extract @_;
  
  my $string = join ' ' => @inputs; 

  return "qq\{$string\}";
}


sub say_string {
  my $class = shift;
  my (@inputs) = extract @_;
  
  my $string = join ' ' => @inputs; 
 

  return "q\{$string\}";
}


sub say_word {
  my $class = shift;
  my ($quote) = extract @_;
  
  return "$quote";
}


sub say_meta {
  my $class = shift;
  my ($meta) = extract @_;
  
  given ( $meta ) {
    $meta = '\t' when /tab/i;
    $meta = '\n' when /newline/i;
    default {
      $meta = "\$$meta";
      #$meta = $variables{ "$name" } // "$name";
    }
  }

  return "$meta";
}


sub say_op_word {
  my $class = shift;
  my ($op) = extract @_;
  
  given ( $op ) {
    $op = '+'   when /plus/i;
    $op = '-'   when /minus/i;
    $op = '%'   when /modulo/i;
    $op = '*'   when /times/i;
    $op = '.'   when /joined/i;
    $op = '/'   when /divided/i;
    $op = '*'   when /multiplied/i;
    $op = 'x'   when /repeated/i;
    $op = 'cmp' when /compared/i;
    $op = '<=>' when /evaluated/i;
    $op = '..'  when /to/i;
    $op = '**'  when /the/i;
    $op = '<<'  when /left/i;
    $op = '>>'  when /right/i;
    default { die "NOT A VALID DIRECTION TO BITSHFIT ($op)" }
  }

  return "$op"
}


sub adjust_sorted {
  my $class = shift;
  my (@inputs) = extract @_;
  
  my $output = shift @inputs;
  $output => qq|\{ $inputs[0] \} $output| if @inputs;

  return "( sort $output )";
}


sub adjust_phrase {
  my $class = shift;
  my ($target, $oper, $value) = extract @_;
  
  given ( $oper ) {
    $oper = '.' when /joined/i;
    $oper = '/' when /divided/i;
    $oper = '*' when /multiplied/i;
    $oper = 'cmp' when /compared/i;
    $oper = '<=>' when /evaluated/i;
    default {}
  }

  return "( $target $oper $value )";
}


sub adjust_incrmt {
  my $class = shift;
  my ($target, $incr) = extract @_;
  
  given ( $incr ) {
    $target = "++$target" when /incremented/i;
    $target = "--$target" when /decremented/i;
    $target = "$target++" when /post-incremented/i;
    $target = "$target--" when /post-decremented/i;
    default {}
  }

  return "$target";
}


sub adjust_pragma {
  my $class = shift;
  my ($expression, $keyword, $value) = extract @_;
  
  my $reset;
  given ( $keyword ) {
    $reset = "no" when /using/i;
    $reset = "use" when /no/i;
  }

  return "$keyword $value; $expression; $reset $value";
}


sub adjust_negate {
  my $class = shift;
  my ($value) = extract @_;
  
  return "( not $value )";
}