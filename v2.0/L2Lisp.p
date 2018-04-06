program L2Lisp (input, output, l2init);
{ Little Lambda Lisp                                       H19.5/30 (鈴)

標準 Pascal (ISO 7185) で書いた小さな Lisp インタープリタ:
ラムダ算法を Scheme と同程度によく近似する Emacs Lisp もどき

コンパイル例: gpc --transparent-file-names -Wall --classic-pascal L2Lisp.p

起動すると，ファイル l2init に書かれた S 式を順に評価してからコンソールでの
対話入力を待つ。defun, defmacro, let 等は l2init でマクロとして定義される。

特徴:
* 字句的スコープ
* 末尾呼出しの最適化
* 関数と変数の名前空間は同一
* シンボルの初期値は (未束縛ではなく) 自分自身
* (dump) は内部状態を印字して nil を返す。
* (eval e) は e を大域的な環境で評価する。
* (lambda …) を評価すると再帰的に内部形式のラムダ式になる。
* (macro …) は大域的な環境でだけ評価できる。結果はマクロ式である。
  マクロ式はラムダ式と同様だが，適用時，引数を評価せず，適用結果を評価する。
* (lambda …) を評価する時，最大 MaxMacroExps 重だけ再帰的にマクロ展開する。
  このとき非大域的に束縛されたマクロは展開されない。
* 印字するとき，高々 MaxExpansions 重だけ再帰的に印字済みリストを印字する。
* 文字列は先頭がシンボル *string*，それ以降が文字コード整数のリストである。
* 先頭がシンボル *symbol*, 次が文字列，その次が任意の値である長さ３のリスト
  は，第３要素を大域的な変数値とする uninterned symbol として扱える。
* その他の点では Emacs Lisp のサブセット
}

const
   MaxCells      = 1500000;
   MaxStack      = 500000;
   MaxSymbols    = 10000;
   NameLen       = 16;
   MaxMacroExps  = 20;
   MaxExpansions = 5;
   NilVal        = 0;
   {
   Lisp 式の値 i を次のように割り振る。
   (MaxCells+1 <= i)                : 非負整数 (i-MaxCells-1)
   (1 <= i) and (i <= MaxCells)     : cons セル cell[i]
   (i = 0)                          : nil 値
   (-MaxSymbols <= i) and (i <= -1) : シンボル symbol[-i]
   (i <= -MaxSymbols-1)             : 負整数 (i+MaxSymbols)
   シンボルのうち -MaxInners .. -1 は内部実装シンボルとする。
   }
   EofVal = -1;
   ErrorVal = -2;
   UncaughtVal = -3;
   LambdaImp = -4; MacroImp = -5; ArgImp = -6; ClosureImp = -7;
   MaxNative = -11;
   CarImp = -11; CdrImp = -12; ConsImp = -13; AtomImp = -14; EqImp = -15;
   Prin1Imp = -16; PrincImp = -17; TerpriImp = -18; ReadImp = -19;
   DumpImp = -20; AssqImp = -21;
   PlusImp = -22; DiffImp = -23; TimesImp = -24; QuotientImp = -25;
   LessThanImp = -26;
   EvalImp = -27; RplacaImp = -28; RplacdImp = -29;
   ThrowImp = -30;
   MaxInners = 30;

type
   ValType    = integer; { Lisp 式の値 }
   NameType   = packed array [1..NameLen] of char;
   CellType   = record
                   car, cdr : ValType
                end;
   SymbolType = record
                   val   : ValType;
                   inUse : boolean;
                   name  : NameType
                end;

var
   l2init    : Text;
   cell      : array [1..MaxCells] of CellType;
   marked    : packed array [1..MaxCells] of boolean; { GC 用 }
   symbol    : array [1..MaxSymbols] of SymbolType;   { ハッシュ表 }
   stack     : array [1..MaxStack] of ValType;
   sp        : 0..MaxStack;         { スタックポインタ，空のときは 0 }
   freelist  : ValType;             { 自由リスト }
   environ   : ValType;             { 環境リスト }
   throwntag : ValType;
   thrownval : ValType;
   QuoteVal, TVal, RestVal, StringVal, SymbolVal,
   CondVal, SetqVal, LambdaVal, MacroVal,
   CatchVal, UnwindProtectVal: ValType;


{ ----------------------------- 基本操作 ----------------------------- }

function IsCardVal(i : ValType): boolean;
begin IsCardVal := MaxCells < i end;

function IsCell(i : ValType): boolean;
begin IsCell := (1 <= i) and (i <= MaxCells) end;

function IsSymbol(i : ValType): boolean;
begin IsSymbol := (-MaxSymbols <= i) and (i <= -1) end;

function IsNegVal(i : ValType): boolean;
begin IsNegVal := i < -MaxSymbols end;


function IsUSymbol(i : ValType): boolean; { uninterned symbol か？ }
begin
   if IsCell(i) then
      IsUSymbol := (cell[i].car = SymbolVal)
   else
      IsUSymbol := false
end; { IsUSymbol }


function ToCardVal(n : integer): ValType;
begin ToCardVal := n + MaxCells + 1 end;

function ToCardinal(i : ValType): integer;
begin ToCardinal := i - MaxCells - 1 end;


function ToNegVal(n : integer): ValType;
begin ToNegVal := n - MaxSymbols end;

function ToNegative(i : ValType): integer;
begin ToNegative := i + MaxSymbols end;


function ToIntVal(n : integer): ValType;
begin
   if n < 0 then ToIntVal := ToNegVal(n) else ToIntVal := ToCardVal(n)
end;

function ToInteger(i : ValType): integer;
begin
   if IsNegVal(i) then ToInteger := ToNegative(i)
   else ToInteger := ToCardinal(i)
end;


{ 文字列からシンボル値を得る }
function MakeSymbol(var name : NameType): ValType;
label 10, 20; 
var h, i : integer;
begin 
   { 思いつきのハッシュ関数…要検証！ }
   h := 1 + MaxInners + (ord(name[1]) * 353 + ord(name[2]) * 13 +
                         ord(name[3]) * 3 + ord(name[4]) * 103 +
                         ord(name[5])) mod (MaxSymbols - MaxInners);
   i := h;
   while symbol[i].inUse do begin
      if name = symbol[i].name then goto 10; { 登録済みだった }
      i := i + 1;
      if i > MaxSymbols then i := 1 + MaxInners;
      if i = h then begin
         MakeSymbol := NilVal;               { 表があふれた }
         goto 20
      end
   end;
   { 新しいシンボルを作る (初期値は自分自身とする) }
   symbol[i].val := -i;
   symbol[i].inUse := true;
   symbol[i].name := name;
10:
   MakeSymbol := -i;
20:
end; { MakeSymbol }


procedure Initialize; { 大域変数を初期化する }
var
   i : integer;
   s : NameType;

   procedure MakeInnerSymbol(var t : NameType; j : ValType);
   begin
      with symbol[-j] do begin
         val := j;
         inUse := true;
         name := t
      end
   end; { MakeInnerSymbol }

   procedure SetSymbol(var t : NameType; j : ValType);
   var k : ValType;
   begin
      k := MakeSymbol(t);
      symbol[-k].val := j
   end; { SetSymbol }

begin 
   { すべてのセルを自由リストにつなげる }
   for i := 1 to MaxCells - 1 do cell[i].car := i + 1;
   cell[MaxCells].car := NilVal; { 自由リストの終端を nil にする }
   freelist := 1;
   { 環境リスト等を nil にする }
   environ := NilVal;
   throwntag := NilVal;
   thrownval := NilVal;
   { スタックを空にする }
   sp := 0;
   { シンボル領域を設定する }
   for i := 1 to MaxSymbols do symbol[i].inUse := false;
        {1234567890123456}
   s := 'quote           '; QuoteVal := MakeSymbol(s);
   s := 't               '; TVal := MakeSymbol(s);
   s := '&rest           '; RestVal := MakeSymbol(s);
   s := '*string*        '; StringVal := MakeSymbol(s);
   s := '*symbol*        '; SymbolVal := MakeSymbol(s);
   s := 'cond            '; CondVal := MakeSymbol(s);
   s := 'setq            '; SetqVal:= MakeSymbol(s);
   s := 'lambda          '; LambdaVal := MakeSymbol(s);
   s := 'macro           '; MacroVal := MakeSymbol(s);
   s := 'catch           '; CatchVal := MakeSymbol(s);
   s := 'unwind-protect  '; UnwindProtectVal := MakeSymbol(s);

   s := '#<eof>          '; MakeInnerSymbol(s, EofVal);
   s := '#<error>        '; MakeInnerSymbol(s, ErrorVal);
   s := '#<uncaught>     '; MakeInnerSymbol(s, UncaughtVal);
   s := '#<lambda>       '; MakeInnerSymbol(s, LambdaImp);
   s := '#<macro>        '; MakeInnerSymbol(s, MacroImp);
   s := '#<arg>          '; MakeInnerSymbol(s, ArgImp);
   s := '#<closure>      '; MakeInnerSymbol(s, ClosureImp);

   s := '#<car>          '; MakeInnerSymbol(s, CarImp);
   s := '#<cdr>          '; MakeInnerSymbol(s, CdrImp);
   s := '#<cons>         '; MakeInnerSymbol(s, ConsImp);
   s := '#<atom>         '; MakeInnerSymbol(s, AtomImp);
   s := '#<eq>           '; MakeInnerSymbol(s, EqImp);
   s := '#<prin1>        '; MakeInnerSymbol(s, Prin1Imp);
   s := '#<princ>        '; MakeInnerSymbol(s, PrincImp);
   s := '#<terpri>       '; MakeInnerSymbol(s, TerpriImp);
   s := '#<read>         '; MakeInnerSymbol(s, ReadImp);
   s := '#<dump>         '; MakeInnerSymbol(s, DumpImp);
   s := '#<assq>         '; MakeInnerSymbol(s, AssqImp);
   s := '#<+>            '; MakeInnerSymbol(s, PlusImp);
   s := '#<->            '; MakeInnerSymbol(s, DiffImp);
   s := '#<*>            '; MakeInnerSymbol(s, TimesImp);
   s := '#</>            '; MakeInnerSymbol(s, QuotientImp);
   s := '#<lt>           '; MakeInnerSymbol(s, LessThanImp);
   s := '#<eval>         '; MakeInnerSymbol(s, EvalImp);
   s := '#<rplaca>       '; MakeInnerSymbol(s, RplacaImp);
   s := '#<rplacd>       '; MakeInnerSymbol(s, RplacdImp);
   s := '#<throw>        '; MakeInnerSymbol(s, ThrowImp);
   s := 'car             '; SetSymbol(s, CarImp);
   s := 'cdr             '; SetSymbol(s, CdrImp);
   s := 'cons            '; SetSymbol(s, ConsImp);
   s := 'atom            '; SetSymbol(s, AtomImp);
   s := 'eq              '; SetSymbol(s, EqImp);
   s := 'prin1           '; SetSymbol(s, Prin1Imp);
   s := 'princ           '; SetSymbol(s, PrincImp);
   s := 'terpri          '; SetSymbol(s, TerpriImp);
   s := 'read            '; SetSymbol(s, ReadImp);
   s := 'dump            '; SetSymbol(s, DumpImp);
   s := 'assq            '; SetSymbol(s, AssqImp);
   s := '+               '; SetSymbol(s, PlusImp);
   s := '-               '; SetSymbol(s, DiffImp);
   s := '*               '; SetSymbol(s, TimesImp);
   s := '/               '; SetSymbol(s, QuotientImp);
   s := '<               '; SetSymbol(s, LessThanImp);
   s := 'eval            '; SetSymbol(s, EvalImp);
   s := 'rplaca          '; SetSymbol(s, RplacaImp);
   s := 'rplacd          '; SetSymbol(s, RplacdImp);
   s := 'throw           '; SetSymbol(s, ThrowImp);
end; {Initialize}


procedure CollectGarbage; { 参照されていないセルを回収する }
var
   i    : ValType;
   k, p : integer;

   procedure Mark(j : ValType);
   begin
      if IsCell(j) then
         if not marked[j] then begin
            marked[j] := true;
            p := p + 1;
            stack[p] := j
         end
   end; { Mark }

begin
   for i := 1 to MaxCells do marked[i] := false;
   { スタック，シンボル，環境等から参照されているセルをマークする }
   p := sp;
   for k := 1 to sp do Mark(stack[k]);
   for k := 1 to MaxSymbols do
      if symbol[k].inUse then Mark(symbol[k].val);
   Mark(environ);
   Mark(throwntag);
   Mark(thrownval);
   { セルから参照されているセルをマークする }
   while p > sp do begin
      i := stack[p];
      p := p - 1;
      Mark(cell[i].car);
      Mark(cell[i].cdr)
   end;
   { マークされていないセルを自由リストにつなげる }
   freelist := NilVal;
   for i := MaxCells downto 1 do
      if not marked[i] then begin
         cell[i].car := freelist;
         freelist := i
      end
end; { CollectGarbage }


function MakeCell(car, cdr : ValType): ValType; { 新しくセルを割り当てる }
var i : ValType;
begin
   if freelist = NilVal then CollectGarbage;
   i := freelist;
   freelist := cell[freelist].car;
   cell[i].car := car;
   cell[i].cdr := cdr;
   MakeCell := i
end; { MakeCell }


procedure Push(i : ValType); { 値をスタックに積む }
begin
   sp := sp + 1;
   stack[sp] := i
end; { Push }


function Pop: ValType; { 値をスタックからおろす }
begin
   Pop := stack[sp];
   sp := sp - 1
end; { Pop }


procedure Cons; { スタックから値を二つおろしてセルを作り，それを積む }
begin
   stack[sp - 1] := MakeCell(stack[sp - 1], stack[sp]);
   sp := sp - 1
end; { Cons }


{ ----------------------------- 式の出力 ----------------------------- }

procedure WriteName(s : NameType); { 末尾の空白を省いて名前を印字する }
var i, j, k : integer;
begin
   j := 0;
   for i := 1 to NameLen do
      if s[i] = ' ' then begin
         if j = 0 then j := i
      end
      else begin
         if j > 0 then begin
            for k := j to i - 1 do write('\ ');
            j := 0
         end;
         write(s[i])
      end
end; { WriteName }


procedure WriteExpression(exp : ValType; printQuote : boolean);
{ 引数 exp を Lisp 式として印字する。printQuote ならば引用符を印字する。
循環リスト対策として GC 用の marked を流用する }
var k : integer;

   procedure WriteExp(i : ValType; printQuote : boolean; reclevel : integer);
   label 10, 20, 30;
   var
      flag : boolean;
      j    : ValType;

      function CheckForRecursion: boolean;
      begin
         CheckForRecursion := false;
         if marked[i] then begin
            reclevel := reclevel + 1;
            if reclevel = MaxExpansions then begin
               write('...');
               CheckForRecursion := true
            end
         end
         else marked[i] := true
      end; { CheckForRecursion }

   begin
      if IsCardVal(i) then write(ToCardinal(i): 1)
      else if IsCell(i) then begin
         if cell[i].car = StringVal then begin
            if printQuote then write('"');
            i := cell[i].cdr;
            while IsCell(i) do begin
               if CheckForRecursion then goto 30;
               j := cell[i].car;
               if IsCardVal(j) then write(chr(ToCardinal(j)))
               else begin
                  write('__['); WriteExp(j, true, reclevel); write(']__')
               end;
               i := cell[i].cdr
            end;
            if i <> NilVal then begin
               write('__.['); WriteExp(i, true, reclevel); write(']__')
            end;
30:         if printQuote then write('"')
         end
         else begin
            write('(');
            flag := false;
            repeat
               if CheckForRecursion then goto 20;
               j := cell[i].car;
               if j = StringVal then goto 10;
               if flag then write(' ') else flag := true;
               WriteExp(j, printQuote, reclevel);
               i := cell[i].cdr
            until not IsCell(i);
            if i = NilVal then goto 20;
10:         write(' . '); WriteExp(i, printQuote, reclevel);
20:         write(')')
         end
      end
      else if i = NilVal then write('nil')
      else if IsSymbol(i) then WriteName(symbol[-i].name)
      else write(ToNegative(i): 1)
   end; { WriteExp }

begin
   for k := 1 to MaxCells do marked[k] := false;
   WriteExp(exp, printQuote, 0)
end; { WriteExpression }


procedure Dump; { FOR DEBUGGING }
var
   i    : integer;
   flag : boolean;

   procedure WriteVal(i : ValType);
   begin
      if IsCardVal(i) then write(ToCardinal(i): 1)
      else if IsCell(i) then write('#', i: 1)
      else if i = NilVal then write('nil')
      else if IsSymbol(i) then begin
         WriteName(symbol[-i].name); write('@', -i: 1)
      end
      else write(ToNegative(i): 1)
   end; { WriteVal }

begin
   CollectGarbage;
   for i := 1 to MaxCells do
      if marked[i] then begin
         write(i, ': ');
         WriteVal(cell[i].car); write(', '); WriteVal(cell[i].cdr); writeln
      end;
   write('environ = '); WriteVal(environ);
   write(' = '); WriteExpression(environ, true); writeln;

   write('throwntag = '); WriteVal(throwntag);
   write(', thrownval = '); WriteVal(thrownval); writeln;

   writeln('sp = ', sp: 1);
   write('[');
   flag := false;
   for i := 1 to sp do begin
      if flag then write(', ') else flag := true;
      WriteVal(stack[i])
   end;
   writeln(']');

   for i := 1 to MaxSymbols do
      if symbol[i].inUse then begin
         write(i, ': ', symbol[i].name, ' '); WriteVal(symbol[i].val); writeln
      end
end; { Dump }


procedure DumpStack(count : integer);
label 10;
var i, n : integer;
begin
   for i := 0 to count - 1 do begin
      n := sp - i;
      if n <= 0 then goto 10;
      write(n, ': '); WriteExpression(stack[n], true); writeln
   end;
10:
end; { DumpStack }


{ ----------------------------- 式の入力 ----------------------------- }

procedure ReadExpression(var rf : Text); { S 式を一つ読んで Push する }
label 20, 510, 520;
type
   TokenType = (EofT, BadT, DotT, LParenT, RParenT, QuoteT, IntT, NameT,
                StringT);
var
   ch         : char;
   token      : TokenType;
   name       : NameType;
   num, oldsp : integer;
   oldenv     : ValType;

   procedure ReadToken;      { トークンを読んで token 等をセットする }
   label 10;
   const TAB = 9; FF = 12; CR = 13; SPC = 32;

      function IsNameChar(ch : char): boolean;
      begin
         IsNameChar := ch in ['A'..'Z', 'a'..'z', '0'..'9', '_', '&',
                              '*', '/', '+', '-', '<', '>', '=', '!', '?']
      end; { IsNameChar }
   
      procedure ReadName(prefix : char);
      var i : integer;
      begin
         token := NameT;
         if prefix = ' ' then i := 1
         else begin
            name[1] := prefix;
            i := 2
         end;
         while IsNameChar(ch) do begin
            if i <= NameLen then name[i] := ch;
            i := i + 1;
            get(rf);
            ch := rf^
         end;
         while i <= NameLen do begin
            name[i] := ' ';
            i := i + 1
         end
      end; { ReadName }

      procedure ReadNumber;
      begin
         token := IntT;
         read(rf, num)
      end; { ReadNumber }

      procedure ReadString; { 文字列を読んで Push する }
      var i, n : integer;
      begin
         token := StringT;
         get(rf);           { 最初の " を読み飛ばす }
         read(rf, ch);
         Push(StringVal);
         n := 1;
         while ch <> '"' do begin
            Push(ToCardVal(ord(ch)));
            n := n + 1;
            read(rf, ch)
         end;
         Push(NilVal);
         for i := 1 to n do Cons
      end; { ReadString }

   begin
10:   if eof(rf) then token := EofT
      else begin
         ch := rf^;
         if ord(ch) in [TAB, FF, CR, SPC] then begin { 空白類を読みとばす }
            get(rf);
            goto 10
         end;
         if ch = ';' then begin               { コメントを読みとばす }
            repeat
               get(rf);
               if eof(rf) then goto 10
            until eoln(rf);
            get(rf);
            goto 10
         end;
         if ch = '-' then begin
            get(rf);
            ch := rf^;
            if ch in ['0'..'9'] then begin
               ReadNumber;
               num := -num
            end
            else ReadName('-')
         end
         else if ch in ['0'..'9'] then ReadNumber
         else if IsNameChar(ch) then ReadName(' ')
         else if ch = '"' then ReadString
         else begin
            if ch = '(' then token := LParenT
            else if ch = ')' then token := RParenT
            else if ch = '.' then token := DotT
            else if ch = '''' then token := QuoteT
            else token := BadT;
            get(rf)
         end
      end
   end; { ReadToken }

   procedure ParseExpression; forward;

   procedure ParseListBody; { exp* ["." exp] ")" を読んで Push する }
   begin 
      if token = RParenT then Push(NilVal)
      else begin
         ParseExpression;
         ReadToken;
         if token = DotT then begin
            ReadToken;
            ParseExpression;
            ReadToken;
            if token <> RParenT then begin
               writeln('bad dotted pair');
               goto 520
            end
         end
         else ParseListBody;
         Cons
      end
   end; { ParseListBody }

   procedure ParseExpression; { 式を読んで Push する }
   var i : ValType;
   begin
      case token of
        EofT    : goto 510;
        BadT, DotT,
        RParenT : begin
                     writeln('''', ch, ''' unexpected');
                     goto 520
                  end;
        LParenT : begin
                     ReadToken;
                     ParseListBody
                   end;
        QuoteT  : begin       { 'exp ⇒ (quote . (exp . nil)) }
                     Push(QuoteVal);
                     ReadToken; ParseExpression; Push(NilVal); Cons;
                     Cons
                  end;
        IntT    : if 0 <= num then
                     Push(ToCardVal(num))
                  else
                     Push(ToNegVal(num));
                            {1234567890123456}
        NameT   : if name = 'nil             ' then
                     Push(NilVal)
                  else begin
                     i := MakeSymbol(name);
                     if i = NilVal then begin
                        writeln('too many symbols: ', name);
                        goto 520
                     end;
                     Push(i)
                  end;
        StringT : 
      end
   end; { ParseExpression }

begin
   oldenv := environ;
   oldsp := sp;
   ReadToken;
   ParseExpression;
   goto 20;
510:
   environ := oldenv;
   sp := oldsp;
   Push(EofVal);
   goto 20;
520:
   environ := oldenv;
   sp := oldsp;
   Push(ErrorVal);
20:
end; { ReadExpression }


{ ----------------------------- 式の評価 ----------------------------- }

procedure Eval; { スタックトップを評価値で置き換える }
label 10, 300, 310;
var 
   oldsp  : integer;
   oldenv : ValType;

   procedure EvalTop(canLoseCurrentEnviron : boolean);
   label
      40, 50, 100, 210, 211, 212, 220, 230, 240, 241, 250, 260, 270, 271, 280;
   var
      top, kar, kdr, kaar : ValType;
      argc                : integer;

      function Assq(key, list : ValType): ValType;
      label 20;
      var elem : ValType;
      begin
         while IsCell(list) do begin
            elem := cell[list].car;
            if IsCell(elem) then
               if cell[elem].car = key then goto 20;
            list := cell[list].cdr
         end;
         elem := NilVal;
20:      Assq := elem
      end; { Assq }

      procedure ApplySymbol(sym : ValType);
      var
         arg1, arg2, result : ValType;
         k, m, n            : integer;

         function GetInteger(offset : integer): integer;
         var i : ValType;
         begin
            i := stack[sp - argc + offset];
            if IsCardVal(i) then GetInteger := ToCardinal(i)
            else if IsNegVal(i) then GetInteger := ToNegative(i)
            else goto 210
         end; { GetInteger }

         function GetIntVal(j : integer): ValType;
         var i : ValType;
         begin
            if j >= 0 then begin
               i := ToCardVal(j);
               if not IsCardVal(i) then goto 211
            end
            else begin
               i := ToNegVal(j);
               if not IsNegVal(i) then goto 211
            end;
            GetIntVal := i
         end; { GetIntVal }

      begin
         if (sym < -MaxInners) or (MaxNative < sym) then goto 230;
         arg1 := ErrorVal;
         arg2 := ErrorVal;
         if argc > 0 then begin
            arg1 := stack[sp - argc + 1];
            if argc > 1 then arg2 := stack[sp - argc + 2]
         end;
         case sym of
           CarImp      : if argc <> 1 then goto 250
                         else if arg1 = NilVal then result := NilVal
                         else if IsCell(arg1) then result := cell[arg1].car
                         else goto 280;

           CdrImp      : if argc <> 1 then goto 250
                         else if arg1 = NilVal then result := NilVal
                         else if IsCell(arg1) then result := cell[arg1].cdr
                         else goto 280;

           ConsImp     : if argc <> 2 then goto 250
                         else result := MakeCell(arg1, arg2);

           AtomImp     : if argc <> 1 then goto 250
                         else if IsUSymbol(arg1) then result := TVal
                         else if IsCell(arg1) then result := NilVal
                         else result := TVal;

           EqImp       : if argc <> 2 then goto 250
                         else if arg1 = arg2
                            then result := TVal else result := NilVal;

           Prin1Imp    : if argc <> 1 then goto 250
                         else begin
                            WriteExpression(arg1, true);
                            result := arg1
                         end;

           PrincImp    : if argc <> 1 then goto 250
                         else begin
                            WriteExpression(arg1, false);
                            result := arg1
                         end;

           TerpriImp   : if argc <> 0 then goto 250
                         else begin
                            writeln;
                            result := TVal
                         end;

           ReadImp     : if argc <> 0 then goto 250
                         else begin
                            ReadExpression(input);
                            result := Pop
                         end;

           DumpImp     : if argc <> 0 then goto 250
                         else begin
                            Dump;
                            result := NilVal
                         end;

           AssqImp     : if argc <> 2 then goto 250
                         else result := Assq(arg1, arg2);

           PlusImp     : begin
                            n := 0;
                            for k := 1 to argc do n := n + GetInteger(k);
                            result := GetIntVal(n)
                         end;

           DiffImp     : begin
                            if argc = 0 then n := 0
                            else if argc = 1 then n := - GetInteger(1)
                            else begin
                               n := GetInteger(1);
                               for k := 2 to argc do n := n - GetInteger(k)
                            end;
                            result := GetIntVal(n)
                         end;

           TimesImp    : begin
                            n := 1;
                            for k := 1 to argc do n := n * GetInteger(k);
                            result := GetIntVal(n)
                         end;

           QuotientImp : if argc < 2 then goto 250
                         else begin
                            n := GetInteger(1);
                            for k := 2 to argc do begin
                               m := GetInteger(k);
                               if m = 0 then goto 212;
                               n := n div m
                            end;
                            result := GetIntVal(n)
                         end;

           LessThanImp : if argc <> 2 then goto 250
                         else if GetInteger(1) < GetInteger(2)
                            then result := TVal else result := NilVal;

           EvalImp     : if argc <> 1 then goto 250
                         else begin
                            Push(environ);
                            environ := NilVal; { 大域的な環境にする }
                            Push(arg1);
                            EvalTop(true);
                            result := Pop;
                            environ := Pop
                         end;

           RplacaImp   : if argc <> 2 then goto 250
                         else if not IsCell(arg1) then goto 280
                         else begin
                            cell[arg1].car := arg2;
                            result := arg2
                         end;

           RplacdImp   : if argc <> 2 then goto 250
                         else if not IsCell(arg1) then goto 280
                         else begin
                            cell[arg1].cdr := arg2;
                            result := arg2
                         end;

           ThrowImp    : if argc <> 2 then goto 250
                         else begin
                            throwntag := arg1;
                            thrownval := arg2;
                            goto 310
                         end
         end;
         stack[sp - argc] := result;
         sp := sp - argc
      end; { ApplySymbol }


      procedure MakeClosure(newkar, j : ValType); { j = (arity . body) }
      begin
         Push(newkar);
         Push(cell[j].car);
         Push(environ);                        { newkar  arity  link }
         Cons;                              { newkar  (arity . link) }
         Push(cell[j].cdr);
         Cons;
         Cons                        { (newkar (ariy . link) . body) }
      end; { MakeClosure }


      procedure ExpandGlobalMacros(j : ValType; count : integer); forward;

      procedure CompileLambda(isLambda : boolean);
      { kdr が指す lambda 式 (または macro 式) をコンパイルする。
      入れ子の lambda 式もコンパイルする。マクロを展開する。
      コンパイル後，束縛変数は (#<arg> level offset . name) に置換される。
      最外の lambda 式と現在の environ から closure を作る。
      スタックトップを closure で置き換える。
      }
      var newkar : ValType;

         procedure Scan(j, alist : ValType); { alist は仮引数名の連想リスト }
         var
            levelVal, offsetVal, k : ValType;
            i, n                   : integer;
         begin
            if IsSymbol(j) or IsUSymbol(j) then begin
               k := Assq(j, alist);
               if k = NilVal then Push(j) { 仮引数以外のシンボルはそのまま }
               else Push(cell[k].cdr)    { (#<arg> 0 offset . name)) }
            end
            else if IsCell(j) then begin
               if cell[j].car = ArgImp then begin
                  j := cell[j].cdr; levelVal := cell[j].car;
                  j := cell[j].cdr; offsetVal := cell[j].car;
                  j := cell[j].cdr;   { = 仮引数名 }
                  k := Assq(j, alist);
                  if k = NilVal then begin
                     Push(ArgImp);
                     Push(levelVal + 1);
                     Push(offsetVal); Push(j); Cons;
                     Cons;
                     Cons       { (#<arg> level+1 offset . 仮引数名) }
                  end
                  else Push(cell[k].cdr) { (#<arg> 0 offset . 仮引数名)) }
               end
               else if cell[j].car in [QuoteVal, MacroImp] then Push(j)
               else begin
                  n := 0;
                  repeat
                     Scan(cell[j].car, alist);
                     n := n + 1;
                     j := cell[j].cdr
                  until not IsCell(j);
                  Push(j);
                  for i := 1 to n do Cons
               end
            end
            else Push(j)
         end; { Scan }

         function ArgList(i : ValType): integer; { 仮引数並び -> arity }
         var
            j         : ValType;
            k, offset : integer;
            rest      : boolean;
         begin
            offset := 0;   { 並びの最初の仮引数から順に 0, 1, 2, ... }
            rest := false;
            while IsCell(i) do begin
               if rest then goto 241;
               j := cell[i].car;
               if j = RestVal then begin { &rest rest_arg) }
                  i := cell[i].cdr;
                  if not IsCell(i) then goto 240;
                  j := cell[i].car;
                  if j = RestVal then goto 240;
                  rest := true
               end;
               if IsCell(j) then
                  if cell[j].car = ArgImp then
                     j := cell[cell[cell[j].cdr].cdr].cdr;
               if not (IsSymbol(j) or IsUSymbol(j)) then goto 240;
               Push(j);                  { j = 仮引数名 }
               Push(ArgImp);
               Push(ToCardVal(0)); { 最内のラムダ式だから level は 0 }
               Push(ToCardVal(offset)); Push(j); Cons;
               Cons;
               Cons;
               Cons; { (仮引数名 . (#<arg> level offset . 仮引数名)) }
               offset := offset + 1;
               i := cell[i].cdr { 次の仮引数名へ進む }
            end;
            Push(NilVal); { (仮引数名 . (#<arg> …)) の alist を構成する }
            for k := 1 to offset do Cons;
            if rest then ArgList := -offset else ArgList := offset
         end; { ArgList }

         procedure CompileInners(j : ValType); forward;

         procedure Compile(newcar, j : ValType);
         var arity : integer;
         begin
            if not IsCell(j) then goto 270;
            Push(newcar);
            arity := ArgList(cell[j].car);
            Scan(cell[j].cdr, stack[sp]); { newcar  alist  new-body }
            stack[sp - 1] := ToIntVal(arity); { newcar  arity  new-body }
            ExpandGlobalMacros(stack[sp], MaxMacroExps);
            stack[sp - 1] := stack[sp];
            sp := sp - 1;
            CompileInners(stack[sp]);
            stack[sp - 1] := stack[sp];
            sp := sp - 1;                  { newcar  arity  new-body' }
            Cons;
            Cons                          { (newcar arity . new-body') }
         end; { Compile }

         procedure CompileInners; { (j : ValType) }
         var
            i, n : integer;
            k    : ValType;
         begin
            if IsCell(j) then begin
               k := cell[j].car;
               if k in [QuoteVal, LambdaImp, MacroImp, ArgImp, ClosureImp]
                  then Push(j)
               else if k = LambdaVal then Compile(LambdaImp, cell[j].cdr)
               else if k = MacroVal then goto 271
               else begin
                  n := 0;
                  repeat
                     CompileInners(cell[j].car);
                     n := n + 1;
                     j := cell[j].cdr
                  until not IsCell(j);
                  Push(j);
                  for i := 1 to n do Cons
               end
            end
            else Push(j)
         end; { CompileInners }

      begin
         if isLambda then newkar := LambdaImp else newkar := MacroImp;
         Compile(newkar, kdr);
         stack[sp - 1] := stack[sp];
         sp := sp - 1;
         if isLambda then newkar := ClosureImp
         else if environ <> NilVal then goto 271;
         MakeClosure(newkar, cell[stack[sp]].cdr);
         stack[sp - 1] := stack[sp];
         sp := sp - 1
      end; { CompileLambda }


      procedure ApplyLambda(j : ValType; canLoseOriginalEnviron : boolean);
      { j = ((arity . link) body…)
      スタック上の argc 個の実引数をおろし，link と Cons した環境を構築して
      body… を評価する。スタックトップを末尾の式の評価値で置き換える。
      ただし，第２引数が真ならば，末尾の式に対し，呼出し元の EvalTop の先頭
      へ (元の環境を復元することなく) goto で跳ぶ (⇒ 末尾呼出しの最適化) }
      var
         arity, k, n : integer;
         link, body  : ValType;
      begin
         body := cell[j].cdr;             { (body…) }
         j := cell[j].car;                { (arity . link) }
         arity := ToInteger(cell[j].car);
         link := cell[j].cdr;
         if arity < 0 then begin
            n := -arity -1;               { &rest より前の引数の個数 }
            if argc < n then goto 250
         end
         else begin
            n := arity;
            if argc <> n then goto 250
         end;

         if arity < 0 then begin { rest 引数を１個のリストに構成する }
            Push(NilVal);
            for k := 1 to argc - n do Cons;
            n := n + 1           { 構成したリストを個数に勘定する }
         end;
         Push(NilVal);
         for k := 1 to n do Cons; { スタック上の実引数をリストにする }
         Push(link);
         Cons;                    { ⇒ 新環境 ((arg ...) . link) }
         j := stack[sp];
         stack[sp] := environ;    { 元の環境を退避する }
         environ := j;            { 新環境に変更する }

         if not IsCell(body) then goto 270;
         sp := sp + 1;
         while IsCell(cell[body].cdr) do begin
            stack[sp] := cell[body].car;
            EvalTop(false);
            body := cell[body].cdr
         end;
         if canLoseOriginalEnviron then begin
            sp := sp - 2;         { 元の環境を捨てる }
            stack[sp] := cell[body].car; { 元の式を末尾の式で置き換える }
            goto 100
         end;
         stack[sp] := cell[body].car;
         EvalTop(true);
         stack[sp - 2] := stack[sp];    { 元の式を評価値で置き換える }
         environ := stack[sp - 1];      { 元の環境を復元する }
         sp := sp - 2
      end; { ApplyLambda }


      function BoundVarFor(body : ValType): ValType;
      { (level offset . 仮引数名) に対応する束縛変数を car とするセル }
      var
         k, level, offset : integer;
         link, frame      : ValType;
      begin
         level := ToCardinal(cell[body].car);
         body := cell[body].cdr;
         offset := ToCardinal(cell[body].car);
         link := environ;
         for k := 1 to level do link := cell[link].cdr; { 静的リンクをたどる }
         frame := cell[link].car;             { 該当レベルのフレーム }
         for k := 1 to offset do frame := cell[frame].cdr;
         BoundVarFor := frame         { フレームの offset 番目の cdr }
      end; { BoundVarFor }


      procedure EvalCondBody(body : ValType); { 条件式本体を評価する }
      label 30;
      var clause, result : ValType;
      begin
         while IsCell(body) do begin
            clause := cell[body].car;
            if not IsCell(clause) then begin
               if clause <> NilVal then goto 260
            end
            else begin
               Push(cell[clause].car);        { テスト式 }
               EvalTop(false);
               result := stack[sp];
               if result <> NilVal then begin { テスト結果が真ならば }
                  clause := cell[clause].cdr; { 式の並びを評価する }
                  if not IsCell(clause) then begin { 並びがないときは }
                     sp := sp - 1;
                     stack[sp] := result;
                     goto 30        { テスト結果を条件式の結果とする }
                  end;
                  while IsCell(cell[clause].cdr) do begin
                     stack[sp] := cell[clause].car;
                     EvalTop(false);
                     clause := cell[clause].cdr
                  end;
                  sp := sp - 1;
                  stack[sp] := cell[clause].car; { 末尾の式をトップにして }
                  goto 100                       { 評価する }
               end;
               sp := sp - 1
            end;
            body := cell[body].cdr
         end;
         stack[sp] := NilVal;                   { すべて失敗なら nil }
30:
      end; { EvalCondBody }


      procedure EvalSetqBody(body : ValType); { setq 本体を評価する }
      var result, lval : ValType;
      begin                                   { (LVAL EXP LVAL EXP ..) }
         result := NilVal;
         while IsCell(body) do begin
            lval := cell[body].car;           { LVAL }
            body := cell[body].cdr;
            if not IsCell(body) then goto 250;
            Push(cell[body].car);             { EXP }
            EvalTop(false);
            result := Pop;
            if IsCell(lval) then begin
               if cell[lval].car = ArgImp then begin
                  lval := BoundVarFor(cell[lval].cdr);
                  cell[lval].car := result
               end
               else if cell[lval].car = SymbolVal then
                  cell[cell[cell[lval].cdr].cdr].car := result
               else goto 240
            end
            else if IsSymbol(lval) then symbol[-lval].val := result
            else goto 240;
            body := cell[body].cdr
         end;
         stack[sp] := result
      end; { EvalSetqBody }


      procedure PushArgs(args : ValType; flag : boolean);
      { 実引数を (flag が真ならば評価して) Push する。個数を argc に与える }
      begin
         argc := 0;
         while IsCell(args) do begin
            Push(cell[args].car);
            if flag then EvalTop(false);
            argc := argc + 1;
            args := cell[args].cdr
         end
      end; { PushArgs }


      procedure ExpandGlobalMacros { (j : ValType; count : integer) };
      { 式 j のマクロを最大 count 重だけ展開する }
      label 60;
      var
         k    : ValType;
         i, n : integer;
      begin
         if (count > 0) and IsCell(j) then begin
            k := cell[j].car;
            if k in [QuoteVal, LambdaVal, MacroVal,
                     LambdaImp, MacroImp, ArgImp, ClosureImp] then Push(j)
            else begin
               if IsSymbol(k) then k := symbol[-k].val;
               if IsCell(k) then
                  if cell[k].car = MacroImp then begin
                     Push(NilVal); { ApplyLambda の結果格納用ダミー }
                     PushArgs(cell[j].cdr, false);
                     ApplyLambda(cell[k].cdr, false);
                     ExpandGlobalMacros(stack[sp], count - 1);
                     stack[sp - 1] := stack[sp];
                     sp := sp - 1;
                     goto 60
                  end;
               n := 0;
               repeat
                  ExpandGlobalMacros(cell[j].car, count);
                  n := n + 1;
                  j := cell[j].cdr
               until not IsCell(j);
               Push(j);
               for i := 1 to n do Cons
            end
         end
         else Push(j);
60:
      end; { ExpandGlobalMacros }


      procedure EvalCatchBody(j : ValType); { j = (tag body...) }
      label 70;
      var tag, result : ValType;
      begin
         if not IsCell(j) then goto 250;
         Push(cell[j].car);                 { tag }
         EvalTop(false);
         tag := stack[sp];
         j := cell[j].cdr;
         sp := sp + 1;
         result := NilVal;
         while IsCell(j) do begin
            stack[sp] := cell[j].car;       { body... の各式 }
            Eval;
            result := stack[sp];
            if result = ErrorVal then goto 300
            else if result = UncaughtVal then begin
               if tag <> throwntag then goto 310; { タグは eq で比較 }
               result := thrownval;
               throwntag := NilVal;
               thrownval := NilVal;
               goto 70
            end;
            j := cell[j].cdr
         end;
70:
         sp := sp - 2;
         stack[sp] := result
      end; { EvalCatchBody }


      procedure EvalUnwindProtectBody(j : ValType); { j = (body cleanup...) }
      var result : ValType;
      begin
         if not IsCell(j) then goto 250;
         Push(cell[j].car);                         { body }
         j := cell[j].cdr;
         Eval;
         Push(throwntag);    { cleanup... 内部で完結した throw-catch }
         Push(thrownval);    { に備えてスタックに保存しておく }
         sp := sp + 1;
         while IsCell(j) do begin
            stack[sp] := cell[j].car; { cleanup... の各式 }
            EvalTop(false);
            j := cell[j].cdr
         end;
         sp := sp - 1;
         thrownval := Pop;
         throwntag := Pop;
         result := Pop;
         if result = ErrorVal then goto 300
         else if result = UncaughtVal then goto 310;
         stack[sp] := result
      end; { EvalUnwindProtectBody }


   begin { EvalTop }
100:
      top := stack[sp];
      if IsSymbol(top) then stack[sp] := symbol[-top].val
      else if IsCell(top) then begin
         kar := cell[top].car;
         kdr := cell[top].cdr;
         if kar = QuoteVal then stack[sp] := cell[kdr].car
         else if kar = ArgImp then stack[sp] := cell[BoundVarFor(kdr)].car
         else if kar = CondVal then EvalCondBody(kdr)
         else if kar = SetqVal then EvalSetqBody(kdr)
         else if kar = LambdaVal then CompileLambda(true) { kdr }
         else if kar = MacroVal then CompileLambda(false) { kdr }
         else if kar = LambdaImp then begin
            MakeClosure(ClosureImp, kdr);
            stack[sp - 1] := stack[sp];
            sp := sp - 1
         end
         else if kar in [ClosureImp, StringVal, MacroImp] then { 何もしない }
         else if kar = SymbolVal then
            stack[sp] := cell[cell[cell[top].cdr].cdr].car
         else if kar = CatchVal then EvalCatchBody(kdr)
         else if kar = UnwindProtectVal then EvalUnwindProtectBody(kdr)
         else begin                 { car 部が評価可能ならば評価する }
            if IsSymbol(kar) then kar := symbol[-kar].val
            else if IsCell(kar) then begin
               kaar := cell[kar].car;
               if (kaar <> ClosureImp) and (kaar <> MacroImp) then
                  if kaar = ArgImp then
                     kar := cell[BoundVarFor(cell[kar].cdr)].car
                  else begin
                     Push(kar);
                     EvalTop(false);
                     kar := stack[sp];
                     if IsCell(kar) then begin { kar を GC から保護するため }
                        stack[sp - 1] := kar;
                        stack[sp] := kdr;
                        Cons
                     end
                     else sp := sp - 1
                  end
            end;
            if IsCell(kar) then begin
               if cell[kar].car = ClosureImp then begin
                  PushArgs(kdr, true);
                  ApplyLambda(cell[kar].cdr, canLoseCurrentEnviron)
               end
               else if cell[kar].car = MacroImp then begin
                  PushArgs(kdr, false);
                  ApplyLambda(cell[kar].cdr, false);
                  EvalTop(canLoseCurrentEnviron)
               end
               else goto 220
            end
            else begin
               PushArgs(kdr, true);
               ApplySymbol(kar)
            end
         end
      end;
      goto 50;

280:  write('list expected');
      goto 40;

271:  write('nested macro');
      goto 40;

270:  write('malformed lambda expression');
      goto 40;

260:  write('cond test expected');
      goto 40;

250:  write('arity not matched');
      goto 40;

241:  write('variable unexpected');
      goto 40;

240:  write('variable expected');
      goto 40;

230:  write('not applicable atom');
      goto 40;

220:  write('not applicable list');
      goto 40;

212:  write('division by zero');
      goto 40;

211:  write('overflow');
      goto 40;

210:  write('number expected');
40:   write(': '); WriteExpression(top, true); writeln;
      DumpStack(10);
      goto 300;
50:
   end; { EvalTop }

begin
   oldenv := environ;
   oldsp := sp;
   EvalTop(false);
   goto 10;

310:    { (throw tag value) から到来 }
   environ := oldenv;
   sp := oldsp;
   stack[sp] := UncaughtVal;
   goto 10;

300:
   environ := oldenv;
   sp := oldsp;
   stack[sp] := ErrorVal;
10:
end; { Eval }

{ ----------------------------- 上位制御 ----------------------------- }

procedure ReadEvalLoop(var rf : Text; isInteractive : boolean);
label 10;
var i : ValType;
begin
   while true do begin
      if isInteractive then write('> ');
      ReadExpression(rf);
      i := Pop;
      if i = EofVal then goto 10;
      Push(i);
      Eval;
      i := Pop;
      if isInteractive then begin
         WriteExpression(i, true);
         writeln
      end
   end;
10:
   if isInteractive then writeln('Goodbye.')
end; { ReadEvalLoop }


begin
   Initialize;
   reset(l2init); ReadEvalLoop(l2init, false);
   ReadEvalLoop(input, true)
end.

{
Copyright (c) 2007 Oki Software Co., Ltd.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without 
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND  
NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
}
