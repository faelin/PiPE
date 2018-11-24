package Language::PiPE::Core;

use Filter::Simple;
use Marpa::R2;
use strict;

my $prescan_grammar = Marpa::R2::Scanless::G->new(
  { default_action => '[value]',
    source => \(<<'END_OF_SOURCE'),

:start ::= Schema
Schema ::=
    Rule+ separator => <vertical space char> proper => 0

Rule ::=
    Translate
  | Interpret ('.')
  | Isa ('.')
Translate ::=
    (translate) QUOTEPHRASE (as ':') SCRIPT (';')
Interpret ::=
    (interpret) QUOTEPHRASE (to mean) QUOTEPHRASE
  | QUOTEPHRASE means QUOTEPHRASE
Isa ::=
    QUOTEWORD (is a) WORD
  | QUOTEWORD (is a type of) WORD
  | (each of) QUOTELIST (is a type of) WORD

  NONWORD ~ [^\w;]+
     WORD ~ [\w]+

interpret ~ 'interpret':i
    means ~ 'means':i
     mean ~ 'mean':i
     type ~ 'type':i
     each ~ 'each':i
      and ~ 'and':i
       to ~ 'to':i
       of ~ 'of':i
       is ~ 'is':i
       as ~ 'as':i
       an ~ 'an':i
        a ~ 'a':i

<lquote> ~ [“"]
<rquote> ~ [”"]

<vertical space char> ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]+

:discard ~ whitespace
whitespace ~ [\s]+
END_OF_SOURCE

  }
);

my $parser = Marpa::R2::Scanless::R->new(
  { grammar => $prescan_grammar,
    semantics_package => 'PiPE::Core',
    trace_values => 1,
    trace_terminals => 1,
  }
);


sub extract {
  return map {
    my $node = shift;
    ref $node ? map( extract( $_ ) => @$node ) : $node;
  } @_;
}


FILTER {
    $parser->read( $_ );
    die "Parse is Ambiguous:", $parser->ambiguous() if $parser->ambiguous();

    my $value_ref = $parser->value;   #retrieve evaluated parse-tree

    $value_ref ? return ${$value_ref} : die 'PiPE could not recognize your code.';
      #returns the evaluated parse as the result of the filter, or dies with an error

}
"";   #prevents PiPE from being disabled once initiated
