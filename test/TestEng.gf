concrete TestEng of Test = open NumeralEng, SyntaxEng, ParadigmsEng in
{
  lincat Some = Numeral;
  lin Some1 = makeNumeral "1";
  lin Some2 = makeNumeral "2";
  lin Some3 = makeNumeral "3";
  lin SomeInt x = makeNumeral x.s;
  oper
    makeNumeral : Str -> Numeral = \str ->
      lin Numeral { s = table { _ => table { _ => str }} ; n = plural };
}
