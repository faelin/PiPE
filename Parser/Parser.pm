package PiPE::Parser;

use 5.010;
use Marpa::R2;
use strict;
no warnings 'experimental';

my $source = <<'END_OF_SOURCE';

:start ::=
      PASSAGE
PASSAGE ::=
      SECTION+ separator => <vertical space char> proper => 0
SECTION ::=
      PARAGRAPH (<period>)                      action => say_paragraph
    | (lets talk about) PLAIN_WORD (<period>)   action => say_about
    | (in) PLAIN_WORD (<colon>)                 action => say_language
PARAGRAPH ::=
      SENTENCE_LIST       action => say_sentences
SENTENCE_LIST ::=
      SENTENCE+           separator => <period>   proper => 0  
SENTENCE ::=
      CLAUSE
    | CLAUSE_CHAIN                                action => say_clause_chain
    | BLOCK (<colon>) PARAGRAPH       rank => 1   action => say_block
CLAUSE_CHAIN ::=
      CLAUSE+             separator => <semicolon>   proper => 1
CLAUSE ::=
      NON_CON_CLAUSE
    | NON_CON_CLAUSE CONDITIONAL BOOLEAN   action => say_clause_conditional
NON_CON_CLAUSE ::=
      STATEMENT
    | STATEMENT (<qmark>) NON_CON_CLAUSE (<semicolon>) NON_CON_CLAUSE    action => say_question
STATEMENT ::=
      VERB
    | FLOW
    | VALUE
    | DECLARATION
    | BOOLEAN
    | STATEMENT CONJUNCTION STATEMENT   action => say_statement
CONJUNCTION ::=
      and
    | or
    | xor
BOOLEAN ::=
      VALUE EXISTS                  action => say_boolean
    | VALUE (matched with) REGEX    action => match_regex
BLOCK ::=
      CONDITIONAL NON_CON_CLAUSE                 action => say_conditional
    | do                                         action => say_do
    | (to) PLAIN_WORD                            action => say_sub
    | (otherwise)                                action => say_otherwise
    | (otherwise) CONDITIONAL NON_CON_CLAUSE     action => say_otherwise
    | PLAIN_WORD (does)                          action => say_label
    | PLAIN_WORD (does the following)            action => say_label
    | LOOP_TYPE LOOP                             action => say_loop_block
LOOP ::=
      ACCESSOR
    | (each item in) EXPLICIT_LIST                  action => say_loop
    | (each item called) NAME (in) EXPLICIT_LIST    action => say_loop
    | (each) NAME (in) EXPLICIT_LIST                action => say_ref_loop   # NAME is a ref-type
    | (each) NAME (called) NAME (in) EXPLICIT_LIST  action => say_ref_loop   # First NAME is a ref-type
ACCESSOR ::=
      (the) LIST_COMMAND (from) EXPLICIT_LIST   action => say_accessor
    | (the) ACCESS (of) EXPLICIT_LIST           action => say_accessor
VALUE_LIST ::=
      VALUE
    | EXPLICIT_LIST
NOUN ::=
      NAME                    action => extract_name
   || ARTICLE NAME            action => extract_noun
    | (AN) PLAIN_WORD         action => say_typed_name
   || (AN) SCOPE PLAIN_WORD   action => say_scoped_name
ARTICLE ::=
      AN
    | the
NAME ::=
      (<slquote>) PLAIN_WORD (<srquote>)
   || (<dlquote>) PLAIN_WORD (<drquote>)
    | PLAIN_WORD
VALUE ::=
      NUMBER ([Ee]) <minus> DIGIT   action => say_number
      NUMBER ([Ee]) DIGIT   action => say_number
    | NUMBER
    | VALUE_EXP
    | ADJUSTMENT
    | QUOTE
    | NOUN
NUMBER ::=
      RAW_NUMBER
    | <minus> RAW_NUMBER              action => say_number
RAW_NUMBER ::=
      DIGIT
   || DIGIT (<period>) DIGIT   action => say_decimal
EXISTS ::=
      EXISTENCE PREP_PHRASE                        action => say_exists
    | STATE ADJECTIVE (PREPOSITION) VALUE          action => say_value_chooser
   || STATE (equal to or) ADJECTIVE (than) VALUE   action => say_value_compare
    | (is) defined
    | exists
   || (exists in) EXPLICIT_LIST                    action => say_list_match
EXISTENCE ::=
      EXIST_WORD
    | STATE
STATE ::=
      is
    | (is) not
PREP_PHRASE ::=
      VALUE
    | (PREPOSITION) VALUE
PREPOSITION ::=
      VERB_PREP
    | called
    | about
    | that
    | than
    | over
    | in
    | of
VERB_PREP ::=
      with
    | from
    | into
    | to
    | by
    | times
    | against
VERB ::=
      ACTION_WORD VALUE (VERB_PREP) VALUE                           action => do_binary_op
    | LIST_FUNC VALUE (into) EXPLICIT_LIST                          action => do_list_func
    | (sort) EXPLICIT_LIST                                          action => do_sort_list
   || BLOCK_VERB EXPLICIT_LIST (by <slquote>) CLAUSE (<srquote>)    action => do_list_parse
   || BLOCK_VERB EXPLICIT_LIST (by <colon>) PARAGRAPH               action => do_list_parse
    | OUTPUT VALUE_LIST                                             action => do_output
   || OUTPUT VALUE_LIST (to) NAME                                   action => do_output
    | UNARY VALUE                                                   action => do_unary_op
    | LIST_COMMAND NAME                                             action => do_list_access
    | reset
   || reset (<slquote>) [\w\-] (<srquote>)
    | (bitshift) VALUE DIRECTION (by) VALUE                      action => do_bitshift
    | (seek) VALUE (bytes from the) DIRECTION (of) NAME          action => do_seek
   || (seek) VALUE (bytes from the) current (position in) NAME   action => do_seek
    | (do) FUNCTION                                              action => do_function
FLOW ::=
      (be done)                   action => say_last
   || (be done with) PLAIN_WORD   action => say_last
   || (be done with this)         action => say_last
    | (do the next one)           action => say_next
   || (do the next) PLAIN_WORD    action => say_next
    | FLOW_WORD (this)            action => say_flow
   || FLOW_WORD PLAIN_WORD        action => say_flow
    | (go to) PLAIN_WORD          action => say_goto
EXPLICIT_LIST ::=
      (<lparen>) COMMA_LIST (<rparen>)   action => flatten_list
    | IMPLICIT_LIST                      action => flatten_list
    | NAME                               action => extract_list
IMPLICIT_LIST ::= 
      COMMA_LIST
    | PLAIN_WORD (paired to) VALUE_LIST      action => say_pair   assoc => right
    | NAME BLOCK_OP (by) CLAUSE              action => say_list_parse
   || NAME BLOCK_OP (by <colon>) PARAGRAPH   action => say_list_parse
COMMA_LIST ::=
      VALUE                          action => say_list
    | VALUE (<comma>) COMMA_LIST     action => say_list
    | VALUE (<comma> and) VALUE      action => say_list
VALUE_EXP ::=
      VALUE OPERATOR VALUE   action => say_op_exp   assoc => right
    | NAME (as) AN NAME      #action => say_interface
    | ACCESSOR
    | ASSIGNMENT
ASSIGNMENT ::=
      NAME (gets) VALUE_LIST                         action => var_assign
    | NOUN (called) NAME (gets) VALUE_LIST           action => post_declare_var
   || NOUN (that gets) VALUE_LIST (is called) NAME   action => post_assign_declare
DECLARATION ::=
      NAME (is) NOUN                                              action => declare_var
   || NAME EXPLICIT_LIST                                          action => declare_list          assoc => right
    | NAME (does) FUNCTION                                        action => declare_sub
   || NAME (is) NOUN (that) FILE_VERB FILE_NAME                   action => declare_filehandle
    | NAME FILE_VERB FILE_NAME                                    action => open_filehandle
   || ARTICLE ('scalar' called) NAME FILE_VERB FILE_NAME          action => open_filehandle
   || ARTICLE SCOPE ('scalar' called) NAME FILE_VERB FILE_NAME    action => open_filehandle
FILE_VERB ::=
      reads and writes to   action => say_file_verb
   || reads and writes      action => say_file_verb
    | reads from            action => say_file_verb
   || reads                 action => say_file_verb
    | pipes from            action => say_file_verb
   || pipes to              action => say_file_verb
    | writes to             action => say_file_verb
   || writes                action => say_file_verb
    | appends to            action => say_file_verb
   || appends               action => say_file_verb
FILE_NAME ::=
      (<lquotes>) FILE_PATH <period> PLAIN_WORD (<rquotes>)                           action => say_file_name
   || (<lquotes>) <slash> FILE_PATH <period> PLAIN_WORD (<rquotes>)                   action => say_file_name
    | (<lquotes>) [A-Za-z] <colon> <slash> FILE_PATH <period> PLAIN_WORD (<rquotes>)  action => say_file_name
FILE_PATH ::=
      PLAIN_WORD+   separator => <slash> proper => 0
REGEX ::=
      (<foreslash>) REGEXP_PHRASE (<foreslash>)   action => say_regex
REGEXP_PHRASE ::=
      REGEX_WORD*
QUOTE ::=
      (<dlquote>) TEXT (<drquote>)        action => say_quote
TEXT ::=
      STRING*
STRING ::=
      (<lsquare>) META (<rsquare>)
    | (<slquote>) SMALL_STRING (<srquote>)  action => say_string
    | QUOTE_WORD                            action => say_word
SMALL_STRING ::=
      SMALL_WORD*
META ::=
      NAME        action => say_meta
    | tab         action => say_meta
    | newline     action => say_meta
OPERATOR ::=
      OP_WORD                      action => say_op_word
    | OP_VERB (VERB_PREP)          action => say_op_word
    | to                           action => say_op_word
    | (to) the                     action => say_op_word
   || (to) the (power of)          action => say_op_word
   || (bitshifted) DIRECTION (by)  action => say_op_word
ADJUSTMENT ::=
      EXPLICIT_LIST (<lsquare> sorted <rsquare>)                            action => adjust_sorted
    | EXPLICIT_LIST (<lsquare> sorted by) VALUE_EXP (<rsquare>)             action => adjust_sorted
    | VALUE         (<lsquare>) OP_VERB (VERB_PREP) VALUE_EXP (<rsquare>)   action => adjust_phrase
    | VALUE         (<lsquare>) ADJUST_WORD (<rsquare>)                     action => adjust_incrmt
    | VALUE         (<lsquare>) using PLAIN_WORD (<rsquare>)                action => adjust_pragma
    | VALUE         (<lsquare>) no PLAIN_WORD (<rsquare>)                   action => adjust_pragma
    | VALUE         (<lsquare>) in PLAIN_WORD (<rsquare>)                   action => adjust_pragma
    | (not) VALUE                                                           action => adjust_negate
FUNCTION ::=
      PLAIN_WORD                        action => say_function
    | PLAIN_WORD (with) EXPLICIT_LIST   action => say_function



LIST_COMMAND ~ 'shift':i | 'pop':i

 ACTION_WORD ~ 'add':i | 'subtract':i | 'multiply':i | 'divide':i | 'join':i | 'repeat':i | 'match':i

 ADJUST_WORD ~ 'incremented':i | 'decremented':i | 'post-incremented':i | 'post-decremented':i

 CONDITIONAL ~ 'if':i | 'when':i | 'unless':i | 'while':i | 'until':i

  BLOCK_VERB ~ 'map':i | 'select':i

  EXIST_WORD ~ 'has':i | 'exists':i | 'equals':i | 'matches':i

   ADJECTIVE ~ 'lesser':i | 'greater':i | 'less':i | 'more':i

   DIRECTION ~ 'left':i | 'right':i | 'beginning':i | 'end':i

   FLOW_WORD ~ 'stop':i | 'redo':i | 'dump':i | 'goto':i

   LIST_FUNC ~ 'push':i | 'unshift':i

   LOOP_TYPE ~ 'for':i | 'given':i

    BLOCK_OP ~ 'sorted':i | 'mapped':i | 'selected':i

     OP_VERB ~ 'joined':i | 'divided':i | 'multiplied':i | 'compared':i | 'evaluated':i | 'repeated':i

     OP_WORD ~ 'plus':i | 'minus':i | 'modulo':i | 'times':i
 
      ACCESS ~ 'top':i | 'bottom':i | 'keys':i | 'values':i

      OUTPUT ~ 'print':i | 'say':i | 'pronounce':i

       UNARY ~ 'increment':i | 'decrement':i

       SCOPE ~ 'local':i | 'global':i

          AN ~ 'a':i | 'an':i

 
   following ~ 'following':i
  bitshifted ~ 'bitshifted':i
   otherwise ~ 'otherwise':i
    bitshift ~ 'bitshift':i
    position ~ 'position':i
     against ~ 'against':i
     defined ~ 'defined':i
     current ~ 'current':i
     matched ~ 'matched':i
     newline ~ 'newline':i
     appends ~ 'appends':i
      paired ~ 'paired':i
      called ~ 'called':i
      sorted ~ 'sorted':i
      writes ~ 'writes':i
      exists ~ 'exists':i
       pipes ~ 'pipes':i
       using ~ 'using':i
       about ~ 'about':i
       equal ~ 'equal':i
       bytes ~ 'bytes':i
       reset ~ 'reset':i
       power ~ 'power':i
       times ~ 'times':i
       reads ~ 'reads':i
        sort ~ 'sort':i
        next ~ 'next':i
        does ~ 'does':i
        item ~ 'item':i
        than ~ 'than':i
        each ~ 'each':i
        talk ~ 'talk':i
        done ~ 'done':i
        with ~ 'with':i
        seek ~ 'seek':i
        from ~ 'from':i
        into ~ 'into':i
        that ~ 'that':i
        over ~ 'over':i
        this ~ 'this':i
        gets ~ 'gets':i
        lets ~ 'let':i ['] 's':i
         one ~ 'one':i
         the ~ 'the':i
         not ~ 'not':i
         and ~ 'and':i
         xor ~ 'xor':i
         tab ~ 'tab':i
          no ~ 'no':i
          of ~ 'of':i
          do ~ 'do':i
          or ~ 'or':i
          as ~ 'as':i
          to ~ 'to':i
          go ~ 'go':i
          in ~ 'in':i
          is ~ 'is':i
          be ~ 'be':i
          by ~ 'by':i
 

 <dlquote>   ~ [“"]
 <drquote>   ~ [”"]
 <slquote>   ~ [‘']
 <srquote>   ~ [’']
 <lbrack>    ~ '{'
 <rbrack>    ~ '}'
 <lsquare>   ~ '['
 <rsquare>   ~ ']'
 <lparen>    ~ '('
 <rparen>    ~ ')'
 <semicolon> ~ ';'
 <qmark>     ~ '?'
 <period>    ~ '.'
 <comma>     ~ ','
 <colon>     ~ ':'
 <plus>      ~ '+'
 <minus>     ~ '-'
 <star>      ~ '*'
 <slash>     ~ '/'
 <backslash> ~ '\'
 <modulo>    ~ '%'


<squotes>   ::=     <slquote> | <srquote>
<dquotes>   ::=     <dlquote> | <drquote>
<lquotes>   ::=     <slquote> | <dlquote>
<rquotes>   ::=     <srquote> | <drquote>
<parens>    ::=      <lparen> | <rparen>
<slash>     ::=       <slash> | <backslash>

<mathsym>   ::=        <plus> | <minus> | <slash> | <modulo> | <star>                            #math symbol set
CHAR_SET    ::=    RSVRD_CHAR | <qmark> | <colon> | <period> | <comma> | <semicolon>             #punctuation set

REGEX_WORD  ::=    PLAIN_WORD | NON_WORD   | <squotes>  | <dquotes>   | <backslash> | <parens>   #no foreslash
QUOTE_WORD  ::=    PLAIN_WORD | NON_WORD   | <squotes>  | <foreslash>                            #no double quotes
SMALL_WORD  ::=    PLAIN_WORD | NON_WORD   | <dquotes> | <foreslash>                             #no single quotes
#WORD_THING ::=    REGEX_WORD | QUOTE_WORD | SMALL_WORD
NON_WORD    ::=    CHAR_SET+


PLAIN_WORD    ~   [\w]+                          #matches [A-Za-z0-9_]
RSVRD_CHAR    ~   [^\w{}\[\]()“”"/\\‘’'?.,:;]+   #matches non-reserved non-word characters
DIGIT         ~   [0-9_]+                        #matches any integer number



<vertical space char> ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]+

# allow whitespace
:discard    ~   whitespace
whitespace  ~   [\s]+

# allow comments
:discard          ~   <comment>
<comment>         ~   <lbrack> <comment body> <rbrack>
<comment body>    ~   <comment string>*
<comment string>  ~   <comment> | <comment char>
<comment char>    ~   [^{}]


END_OF_SOURCE





my $grammar = Marpa::R2::Scanless::G->new(
  { default_action => '[value]',
    source => \$source
  }
);

my $parser = Ma
rpa::R2::Scanless::R->new(
  { grammar => $grammar,
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
my $value = $value_ref ? ${$value_ref} : 'PiPE could not recognize your code.';


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