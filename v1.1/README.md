# Little Lambda Lisp in Standard Pascal

May 2007 (Edit: April 2018) by (鈴)

<a name="1"></a>
## 1. はじめに

L2Lisp (Little Lambda Lisp) プログラムは，著者が __標準 Pascal__
で書いた小さな (ソース行数でいえば約 1,400 行の) Lisp インタープリタである。
名前の由来はインタープリタがラムダ算法を (Scheme 
と同じ程度に) よく近似することである。
その意味論は下記の点でラムダ算法に接近している。


  1. 字句的スコープ (静的スコープ)
  2. 末尾呼出しの最適化
  3. 関数と変数の名前空間は同一


これ以外の点では __おおむね Emacs Lisp のサブセット__ とした。現在の
Emacs Lisp がおおむね動的スコープの Common Lisp サブセット
(本来の Common Lisp は基本的に字句的スコープ) であることから，
L2Lisp は，(Lisp-2 ではなく) Lisp-1 
の超小型 Common Lisp サブセットといえる。

実装を簡単にするため，数値型を整数に限り，
cons セルとシンボルを表現するために値域を狭めた。
ガーベジコレクタは実時間的ではなく，
ヒープ領域の動的な拡大はしない。


他の特徴および注意点を示す。

  4. シンボルの初期値は (未束縛ではなく) 自分自身である。
  5. (dump) は内部状態を印字して nil を返す。
  6. (eval <var>e</var> ) は <var>e</var> を大域的な環境で評価する。
  7. (make-symbol <var>name</var> ) は 
     <var>name</var> という名のインターン済み (← 注意！) シンボルを作る。
  8. (lambda …) を評価すると内部形式の手続きになる。
  9. (macro …) は (lambda …) と同様。ただし引数を評価せず，結果を評価する。
 10. (lambda …) を評価して手続きを作る時，そこで有効な大域変数が，大域的な
     macro 式に束縛されているならば，
     一定レベルまであらかじめ再帰的にマクロ展開する 
     (→ 無限に展開するマクロ定義でも破綻しない)。
 11. 循環リストを印字するとき，一定レベルだけ印字済みリストを再帰的に印字する
     (→ リスト構造に循環があっても破綻しない)。
 12. 文字列は，先頭要素がシンボル `*string*`，それ以降の要素が文字コード整数の
     リストである。

<a name="2"></a>
## 2. コンパイル方法と実行方法

プログラムの記述は **ISO 7185**, 
**JIS X 3008** のいわゆる標準 Pascal の規格によった。
標準 Pascal に分割コンパイルの方法はないから，
ソースファイルは１本のファイル [L2Lisp.p](L2Lisp.p) である。
Pascal コンパイラとして [GNU Pascal](http://www.gnu-pascal.de)，
より具体的には [Cygwin](http://www.cygwin.com) 
の標準パッケージに含まれる gcc-gpc 3.4.4-3 を使用した。

Cygwin のシェル上で `L2Lisp.p` をコンパイルする方法は下記のとおりである。

```
$ gpc -O3 --transparent-file-names -Wall --classic-pascal -o llsp L2Lisp.p
L2Lisp.p: In procedure `EvalTop':
L2Lisp.p:680: warning: 'top' might be used uninitialized in this function
$
```

同じディレクトリに `llsp.exe` が作られる。これが目的のインタープリタである。

手続き EvalTop で変数 top が初期化されずに使われるかもしれないという警告が
コンパイラから発せられているが，
実際には手続きの最初の文で top に値を代入しているから，これは安全に無視できる。
もしも初期化されない可能性があるとしたら，それは，
手続きの途中へ手続き間 goto で入り込む場合だが，
規格では (最初の文を経由しているはずの) 入れ子の手続きや関数からしか
手続き間 goto で入り込めないから問題はない。

`L2Lisp.p` は手続き間 goto を使用しているから，
[Free Pascal](http://www.freepascal.org) 2.0.4
などではコンパイルできない。


*  わざわざ悪しき goto を使用してコンパイルできなくするなんて，
   と教条主義的な批判を受ける前に大急ぎで説明しましょう。
   最近の多くの言語では「例外」を使って大域的な脱出をします。
   標準 Pascal では，入れ子の手続き・関数から非局所的に脱出する手段として，
   手続き間 goto が使用できます。
   つまり，手続き間 goto は **「例外」の代替手段** です。
   逆にいえば，いくつかの Pascal 方言および多くの手続き型言語へは，
   「例外」を使うように書き直せば移植できます。


GNU Pascal もまた標準 Pascal をさまざまに拡張している。
--classic-pascal オプションで拡張機能を封印しているが，
この封印は必ずしも完全ではないから，
不注意により GNU Pascal の拡張機能を使用している可能性はある。

* 例えば，文字列変数に文字列定数を代入するとき，
  規格では両者は同じ長さの文字列でなければなりません。
  しかし，GNU Pascal は文字列定数が変数より短いとき，
  コンパイル時に自動的に空白文字を補充します。


`llsp.exe` は起動されると，
ファイル [l2init](l2init)
に書かれた S 式を順に評価してからコンソールでの対話入力を待つ。
defun や defmacro は `l2init` でマクロとして定義される。
`l2init` というファイル名はファイル型のプログラム引数の名前に由来する。
`L2Lisp.p` の先頭を示す。

```Pascal
program L2Lisp (input, output, l2init);
```

GNU Pascal への --transparent-file-names オプションにより，
引数名がそのまま実際のファイル名として使われる。
ただし，厳密にいえば，
ファイル型のプログラム引数と実際のファイルとの結び付け方は処理系定義である。

対話入力で S 式を打鍵すると，その評価結果が表示される。

```
$ ./llsp
> (+ 5 6)
11
> 
```

EOF (普通は Control-D を打鍵) により，インタープリタは入力待ちを終了する。

```
$ ./llsp
> (+ 5 6)
11
> [Control-D]Goodbye.
$ 
```

標準入力のリダイレクトにより，
ファイルに用意した Lisp プログラムを実行させることができる。

* もちろん，これはあまり器用な方法とはいえませんが，
  標準 Pascal でファイルを柔軟に処理することは困難です。
  事情に応じて適宜プログラムに手を入れてください。
  意図としては `L2Lisp.p` はあくまで単なるたたき台であって，
  それぞれ好きに改造，実験，発表されることを期待しています。そのために 
  [MIT ライセンス](L2Lisp.p#L1410-L1433)
  にしてあります。

N クイーン問題を解くプログラム [nqueens.l](../examples/nqueens.l)
の実行例を示す。

```
$ ./llsp < nqueens.l
> node-expand
> safe?
> safe-aux?
> goal?
> nqueens
> ((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
> 92
> Goodbye.
$
```

node-expand 等のシンボルは defun による関数定義の戻り値としての関数名である。
6 個の数のリストを四つ含むリストは 6 クイーン問題の全解である。
92 は 8 クイーン問題の解の個数である。

* nqueens.l は
  猪股・益崎 著「Schemeによる記号処理入門」森北出版 1994年
  の §8.2 を参考にしました。


本インタープリタは決して高速性をねらったものではないが，
それでも (少なくとも場合によっては) かなり高速に動作することが観察できる
([§7](#7) 参照)。


<a name="3"></a>
## 3. データ構造と基本操作

Lisp 処理系の設計の大半はデータ構造の設計である。
いったんデータ構造を決定すると，その上に構築すべき実装は概ね自動的に定まる。
L2Lisp インタープリタの主要な定数，型，変数を示す。

```Pascal
 const
   MaxCells      = 1500000;
   MaxStack      = 500000;
   MaxSymbols    = 10000;
   NameLen       = 16;
```

```Pascal
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
```

```Pascal
 var
    l2init    : Text;
    cell      : array [1..MaxCells] of CellType;
    marked    : packed array [1..MaxCells] of boolean; { GC 用 }
    symbol    : array [1..MaxSymbols] of SymbolType;   { ハッシュ表 }
    stack     : array [1..MaxStack] of ValType;
    sp        : 0..MaxStack;        { スタックポインタ，空のときは 0 }
    freelist  : ValType;            { 自由リスト }
    environ   : ValType;            { 環境リスト }
    throwntag : ValType;
    thrownval : ValType;
```

ValType 型 (実体は integer) の値 _i_ に対し，意味を次のように割り振る。
Lisp 処理系では cons セルの操作が大勢を占めるから，割り振りでは
cons セルを優先し，配列 cell の添字としてそのまま _i_ が使えるようにする。

| ValType 型の値 _i_                   | Lisp としての意味              |
|:-------------------------------------|:-------------------------------|
| MaxCells+1 <= _i_                    | 非負整数 (_i_ - MaxCells - 1)  |
| (1 <= _i_) and (_i_ <= MaxCells)     | cons セル cell[_i_]            |
| _i_ = 0                              | nil 値 (= NilVal)              |
| (-MaxSymbols <= _i_) and (_i_ <= -1) | シンボル symbol[-_i_]          |
| _i_ <= -MaxSymbols-1                 | 負整数 (_i_ + MaxSymbols)      |


* 普通は整数 _i_ の上位または下位ビットに意味を持たせると思いますが，
  標準 Pascal には整数のビット演算がありませんからこうしました。
  配列の添字を 1 から始めていますから太古の FORTRAN 66 にも移植可能です。
  相互再帰する Pascal の手続き群を１個の大きな副プログラムに展開し，
  配列と割当て形 GO TO 文で再帰呼出しをシミュレートする手間をかければ…ですが。


cons セルかどうか判別するために下記の関数を用意する。

```Pascal
function IsCell(i : ValType): boolean;
begin IsCell := (1 <= i) and (i <= MaxCells) end;
```

与えられた car 値と cdr 値から cons セルを作る関数は次のように定義できる。
自由リスト freelist から空のセル (を指す cell の添字)  _i_ を一つ取り出し，
cell[_i_] の car フィールドと cdr フィールドに引数を代入する。
戻り値として _i_ を返す。

```Pascal
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
```

_i_ を取り出すとき，
自由リストが nil ならばガーベジコレクション (GC) を実行する。

```Pascal
procedure CollectGarbage; { 参照されていないセルを回収する }
var
   i : ValType;
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
```

GC は，stack と symbol の有効な配列要素と大域変数 environ, throwntag, thrownval
のいずれからも直接にも間接にも参照されないセルを回収する。
計算途中の値の不用意な回収を防ぐには stack を利用する。

```Pascal
procedure Push(i : ValType); { 値をスタックに積む }
begin
   sp := sp + 1;
   stack[sp] := i
end; { Push }
```

手続き Cons は stack 上で cons セルを構成する。

```Pascal
procedure Cons; { スタックから値を二つおろしてセルを作り，それを積む }
begin
   stack[sp - 1] := MakeCell(stack[sp - 1], stack[sp]);
   sp := sp - 1
end; { Cons }
```

任意の ValType 値 A, B, C から Lisp のリスト (A B C) を構成するには，
次の文を実行すればよい。各文の右にコメントとして事後の stack の内容を記す。
もしも Cons 時に GC が実行されても，A, B, C が直接・間接に指す cons セルは
A, B, C が stack 上にあるから回収されない。

```Pascal
  Push(A);      { A }
  Push(B);      { A B }
  Push(C);      { A B C }
  Push(NilVal); { A B C nil }
  Cons;         { A B (C) }
  Cons;         { A (B C) }
  Cons;         { (A B C) }
```

以上の説明から Lisp の car, cdr, cons, atom, eq 
等の関数の実現方法は明らかであろう。
具体的な実装は `L2Lisp.p` の手続き Eval の内部手続き EvalTop 
の内部手続き 
[ApplySymbol](L2Lisp.p#L723-L887)
を参照されたい。


<a name="4"></a>
## 4. 字句的スコープの実現と末尾呼出しの最適化

モダン Lisp の特徴である字句的スコープは，Scheme が
Algol にならって採用したものである 
([Revised<sup>5</sup> Report on the Algorithmic Language 
Scheme](http://www.unixuser.org/~euske/doc/r5rs-ja/) §1.1)。
Scheme が自らを Algorithmic Language と呼称しているのは，
言語の構文はともかく，意味論的に Algol
([Revised Report on the Algorithmic Language Algol 
60](http://www.masswerk.at/algol60/report.htm))
と近縁であることを暗示しているのであろう。

* つまり，字句的スコープは (意外にも) Lisp 起源というわけではありません。
  ただし，手続き型言語の系統樹のうち最有力の C/C++ 語族は，
  先祖の BCPL が自由な自動変数へのアクセスを禁じて以来，
  その言葉の意味の多くを失っています。
  現在では事実上 Pascal 語族に原形が残されているだけです
  (ラテン語等に典型的に見られる屈折語としての性格が，
  印欧語族で最有力な英語からほとんど失われていることと似ているかもしれません)。

  Lisp (Scheme) のオリジナリティは，字句的スコープを採ったことではなく，
  字句的スコープのもとにある変数が無期限の寿命をもつ，としたことでしょう。
  これは手続きをファーストクラスの値とするために必要です。
  これに対し Pascal には手続き引数はあっても，
  戻り値として手続きを返すことはできません。
  常にスタック上にローカル変数を置けるようにその寿命を限っているからです。
  C/C++ は自由な自動変数を廃止することで，
  手続き値を固定のポインタに単純化して，この問題を (後ろ向きに) 解決しました。

  それでも，以下に述べるように Lisp の字句的スコープは，
  Pascal 等の入れ子手続きでのローカル変数の実現方法と
  基本的に同一の方法で実現できます。
  後者については，例えば
  N. Wirth: "Compiler Construction" の第12章が参考になります。


[§1](#1) の 8. で述べたように，
本処理系でラムダ式のリスト
(lambda 仮引数並び 本体)
を評価して得られる値は
(同じリストへと評価される Emacs Lisp と異なり) 内部形式の手続きである。

内部形式への変換は EvalTop の内部手続き
[CompileLambda](L2Lisp.p#L892-L997)
が行う。
ただし，現在の簡単な実装では本格的なコンパイルはしない。以下の四つを実施する。


 1. コンパイル済みラムダ式と判別できるように，
    先頭のシンボル lambda を内部実装シンボル `#<lambda>` に置き換える。

 2. 仮引数並びを (arity . link) に置き換える。
    ここで arity は引数の個数 (&rest があれば符号反転) であり，
    link はコンパイル時の字句的な環境のリスト environ の値である。

 3. 本体に出現する仮引数 _a_ を
    (`#<arg>` level offset .  _a_)
    に置き換える。
    ここで level はラムダ式の相対的な入れ子深さであり，
    offset はその深さの実引数リストでの仮引数の位置である
    (入れ子手続きがなければ level は不要であり，offset だけでよい。
    その実例は C/C++ でのローカル変数の実現方法等に見られる)。

    もともと (`#<arg>` level offset . _a_) という形式だったならば
    (つまり，そのラムダ式を取り囲むコンパイル済みラムダ式の仮引数だったならば)，
    _a_ が自分自身の仮引数並びに含まれているときは単なる _a_ と同じに扱う
    (つまり，名前が衝突した時は内側を優先する) が，
    そうでなければ level 値を 1 だけ増やす。

 4. 仮引数の (`#<arg>` …) への置換後，
    本体に含まれる式の関数位置にあるシンボル
    (仮引数は置換済みだから，これは大域変数である) が，
    大域的なコンパイル済みマクロ式に束縛されているならば，
    一定レベルまで再帰的に _マクロ展開_ する。

    具体的な実装は EvalTop の内部手続き 
    [ExpandGlobalMacros](L2Lisp.p#L1149-L1203) を参照されたい。
    インタープリタとしてこのステップは省略可能だが，効率のために導入した。


対話入力を受け付けるトップレベルの環境では，字句的な環境のリスト environ 
は nil である。したがって，ラムダ式を対話入力で評価すると下記のようになる。

```
> (lambda (a b) (+ a b))
(#<lambda> (2) (+ (#<arg> 0 0 . a) (#<arg> 0 1 . b)))
```

ここで (2) は (2 . nil) 
つまり引数の個数が２，字句的な環境が空であることを意味する。

コンパイル済みラムダ式の適用は EvalTop の内部手続き 
[ApplyLambda](L2Lisp.p#L1000-L1053)
が行う。
ApplyLambda は stack 上に元の式と実引数が積まれた状態で呼び出される。
ApplyLambda は大略下記を行う。

 1. Push(NilVal) 後，
    実引数の個数だけ Cons を繰り返して，__実引数リスト__ を stack に作る。
    <br>ここでコンパイル済みラムダ式の (arity . link) の arity を使う。

 2. Push(link); Cons をして (__実引数リスト__ . link) を stack に作る。
    <br>ここでコンパイル済みラムダ式の (arity . link) の link を使う。

 3. 現在の大域変数 environ を stack に退避する。
    <br>上記 2. で作った (__実引数リスト__ . link) を新しい environ の値にする。

 4. コンパイル済みラムダ式の __本体__ を評価する。
    <br>このときコンパイル済み仮引数 (`#<arg>` level offset . _a_ ) に対し，
    大域変数 environ の cdr を level 回だけたどり，
    その car である実引数リストの cdr をまた offset 回だけたどって
    実引数へアクセスする (実装は EvalTop 
    の内部関数 [BoundVarFor](L2Lisp.p#L1056-L1070))。

 5. 上記 3. で退避した元の environ 値を大域変数 environ に戻す。
    <br>stack に残された元の式を本体の __末尾の式__ の評価値で置き換える。

実引数，つまりローカル変数の実体を
cons セルで構成したリストで保持している点に注意されたい。
現在の簡単な実装では，変数をスタックに短期間だけ割り付けるなどの最適化はしない。
ローカル変数は常に無期限の (仮想的な) 寿命をもつ。
もちろん実際には，入れ子のラムダ式を大域変数に格納するなどしない限り，
やがて変数の実体は GC で回収される。

ここでもしも最後に元の environ 値を復元しなくてもよいとすれば，
上記の 4. で本体の末尾の式を評価するとき，
評価関数を再帰的に呼び出すかわりに，
評価関数の先頭へ手続き間 goto で大域脱出できる。
本処理系は __末尾呼出しの最適化__ をこうして実現している。
関係する実装コードを示す。

```Pascal
   procedure EvalTop(canLoseCurrentEnviron : boolean);
      ....
   begin { EvalTop }
100:
                  ....
                  ApplyLambda(cell[kar].cdr, canLoseCurrentEnviron)
```

```Pascal
      procedure ApplyLambda(j : ValType; canLoseOriginalEnviron : boolean);
      { j = ((arity . link) body…)
      スタック上の argc 個の実引数をおろし，link と Cons した環境を構築して
      body… を評価する。スタックトップを末尾の式の評価値で置き換える。
      ただし，第２引数が真ならば，末尾の式に対し，呼出し元の EvalTop の先頭
      へ (元の環境を復元することなく) goto で跳ぶ (⇒ 末尾呼出しの最適化) }
         ....
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
```


<a name="5"></a>
## 5. スペシャルフォームと組込み関数のまとめ

L2Lisp.p で実装している Lisp のスペシャルフォームと組込み関数をまとめる。

### スペシャルフォーム

| シンボル       | 引数              | 説明                                    |
|:---------------|:------------------|:----------------------------------------|
| quote          | _arg_             | _arg_ を評価せずに返す。'_arg_ と略記できる。|
| cond           | _clauses..._      | 条件式。各節は (条件 _body_ ...) または (条件)。    すべての節の条件が nil ならば nil。|
| setq           | _sym val ..._     | 代入時に変数 _sym_ が定義されていなければ大域変数として定義する。|
| lambda         | _args body..._    | _args_ は仮引数名からなるリスト。&rest を指定可能。|
| macro          | _args body..._    | (lambda ...) と同じ。ただし適用時に実引数を評価せず，適用結果をさらに評価する。|
| catch          | _tag body..._     | _body..._ 内で (eq で) 等しい _tag_ で (throw _tag_ _value_) が行われたら catch し，_value_ を返す。|
| unwind-protect | _body cleanup..._ | _body_ を評価し，その値を返す。正常にせよ異常にせよ終了時に _cleanup..._ を実行する。|


lambda と macro を除き，これらは Emacs Lisp 
の同名のスペシャルフォームと同じである。
[§1](#1) の 4. に基づき，スペシャルフォームのシンボルはすべて自己評価的である
(例えば，シンボル setq を評価するとシンボル setq になる)。
このシンボルを他の変数に代入しても，
それをそのスペシャルフォームとして使うことはできない。
lambda の実装方法については前節 [§4](#4) を参照されたい。
macro も基本的には同じ方法で実装されている。

### 組込み関数

| シンボル    | 引数            | 説明                                         |
|:------------|:----------------|:---------------------------------------------|
| car         | _list_          | _list_ の car を返す。ただし nil に対しては nil を返す。|
| cdr         | _list_          | _list_ の cdr を返す。ただし nil に対しては nil を返す。|
| cons        | _car cdr_       | _car_ と _cdr_ から新しい cons セルを作る。  |
| atom        | _object_        | _object_ が cons セルでなければ t を返す (したがって nil にも t を返す)。|
| eq          | _obj1 obj2_     | _obj1_ と _obj2_ が同じ Lisp オブジェクトならば t を返す。|
| prin1       | _object_        | _object_ を印字する。改行はしない。循環リストは一定レベルまで印字する。|
| princ       | _object_        | prin1 と同様。ただし文字列を二重引用符で囲まない。|
| terpri      |                 | 改行する (改行文字を印字する)。              |
| read        |                 | Lisp 式を一つ読み込む。                      |
| dump        |                 | デバッグ用にインタープリタの内部状態を印字する。|
| assq        | _key list_      | _key_ と eq で等しい car を持つ _list_ の最初の要素を返す。|
| +           | &rest _numbers_ | _numbers_ の和 (なければ 0) を返す。         |
| -           | &rest _numbers_ | 0引数なら 0。1引数なら正負を逆に。それ以外は第1引数から第2引数以降を引く。|
| *           | &rest _numbers_ | _numbers_ の積 (なければ 1) を返す。         |
| /           | _dividend divisor_ &rest _divisors_ | 第1引数を第2引数以降で割る。|
| <           | _num1 num2_     | 二つの数 _num1 num2_ について _num1_ < _num2_ ならば t。|
| eval        | _form_          | 引数を大域的な環境 (つまり空の字句的環境) で評価する。|
| make-symbol | _name_          | 名前が文字列 _name_ と等しい (インターン済みの) シンボルを作る。|
| rplaca      | _cell newcar_   | _cell_ の car を _newcar_ にし，_newcar_ を返す。|
| rplacd      | _cell newcdr_   | _cell_ の cdr を _newcdr_ にし，_newcdr_ を返す。
| throw       | _tag value_     | _tag_ と _value_ をもって大域脱出する。      |


dump を除き，これらは Emacs Lisp の同名の関数と基本的に同じだが，
下記の点で異なる。

 1. 本処理系と異なり，Emacs Lisp (および Common Lisp) の make-symbol 
    は未インターン・シンボルを作る。
 2. 本処理系 (および Common Lisp) と異なり，Emacs Lisp の eval 
    は引数をその場所の環境で評価する。

関数 rplaca, rplacd の戻り値は Emacs Lisp と同じく第２引数の値である。
一方，Common Lisp は改変後の第１引数の値を戻り値とする。

* dump は，今年 (2007年) ３月後半，処理系づくりをぼつぼつと始めたころ，
  cons セルの動的割付けやシンボルのハッシングが正常に機能しているかどうか
  確かめるため最初に作成したデバッグ用手続きのなごりです。


組込み関数のシンボルは，それぞれ対応する内部実装シンボルに束縛される。
例えば，シンボル car はシンボル `#<car>` に束縛される。
これは，シンボルに基づいて各組込み関数の実装コードへ分岐するところで，Pascal 
の case 文を利用するためである。
内部実装シンボルの ValType 値はコンパイル時定数だから，
case 文の選択肢のラベルとして使うことができる。

* 標準 Pascal にはファーストクラスの関数ポインタ等がありませんから，
  多数の選択肢に対し定数時間での分岐が可能なのは，case 文が (うまく) 
  ジャンプテーブルで実装された場合だけです。
  もしもこのインタープリタを Java/C++ に *直訳で* 移植するときは，
  分岐を実装するために同様に (うまくすればジャンプテーブルへと翻訳される) 
  switch 文が利用できます。

  しかし，Java/C++ には定数時間での高速な分岐を可能にする定番の方法が
  ほかにもありますから，
  本処理系の方法にこだわるのは，必ずしも賢いやりかたとは言えないでしょう。


<a name="6"></a>
## 6. いろいろな関数定義とマクロ定義

前節 [§5](#5) から分かるように，本処理系では，普通，
基本的だと思われている関数やスペシャル・フォームの多くを組み込んでいない。
そのかわり，それらのいくつかを，
初期化ファイル [l2init](l2init)
の中で利用者定義の関数やマクロとして用意している。
ここではそのいくつかを紹介する。

```Lisp
(setq list (lambda (&rest x) x))
```

ラムダ式を適用するとき，rest 引数はリストにまとめられる。
本処理系では関数と変数の名前空間が同じだから，
rest 引数の値を戻り値とするラムダ式を
変数 list に代入すれば，おなじみの list 関数として利用できる。
本処理系の setq は第１引数の変数名が未知ならば新しく大域変数とするから，
結局，上式は事実上，list を大域関数として定義している。

```
> (list 1 2 3)
(1 2 3)
```


複数の式を次々に評価して，最後の式の値を結果とする 
(progn _e<sub>1</sub>_ ... _e<sub>n</sub>_) は，
条件式を使えば (cond (t _e<sub>1</sub>_ ... _e<sub>n</sub>_)) と表現できる。
ラムダ式でも表現できるが，条件式は環境構築等の手間がないから，より軽量である。
macro を使えば，progn は次のように定義できる。

```Lisp
(setq progn (macro (&rest x) (list cond (cons t x))))
```

* 本処理系では cond シンボルを評価した結果は cond シンボル自身ですから
  上式で cond をクォートする必要はありません。

```
> (progn (setq a 10) (+ a a))
20
> a
10
```

これがどのようにマクロ展開されるのかは，
ラムダ式に入れて評価すると実際に見ることができる。

```
> (lambda () (progn (setq a 10) (+ a a)))
(#<lambda> (0) (cond (t (setq a 10) (+ a a))))
```

意図どおりに展開されていることが分かる。

以下，setq で次々とマクロを定義することもできるが，
より慣習に合致させるために defmacro を用意する。

```Lisp
(setq defmacro
      (macro (name args &rest body)
             (list progn
                   (list setq name (cons macro (cons args body)))
                   (list quote name))))
```


ここでは Emacs Lisp の defmacro 
と同じく戻り値として定義対象を返すようにするため，
マクロの定義に必要な list, setq, cons, macro だけでなく，
progn と quote も使った。

* ここで list 関数の引数として出現している progn, setq, quote
  と cons 関数関数の引数として出現している macro 
  は本処理系ではそれぞれ自分自身へと評価されるため，クォートする必要が
  ないことに注意してください。

例えば (defmacro f (x) (foo x)) を実行すると，
(progn (setq f (macro (x) (foo x))) 'f) が評価され，
変数 f がコンパイル済み macro 式に束縛されてマクロとなるとともに，
defmacro 自身の戻り値として f が返される。

```
> (defmacro f (x) (foo x)
f
> f
(#<macro> (1) (foo (#<arg> 0 0 . x)))
```


list, progn, defmacro を用意したから，defun を次のように定義できる。

```Lisp
(defmacro defun (name args &rest body)
  (list progn
        (list setq name (cons lambda (cons args body)))
        (list quote name)))
```

defun を使って次のように関数を定義できる。
本処理系では二つの数が同じ値であるかどうかを eq で判定できるから
「=」 関数は eq で定義できる。

```Lisp
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun null (x) (eq x nil))
(defun not (x) (eq x nil))
(defun print (x) (prin1 x) (terpri) x)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun = (x y) (eq x y))
(defun /= (x y) (not (= x y)))
```


(if _test then else..._)
は
(cond (_test then_) (t _else..._))
と表現できる。
Emacs Lisp (および Common Lisp) では _else..._
が省略されているとき，_test_ が nil ならば
if 式の値も nil であると規定されているから，
(if _test then_)
は
(cond (_test then_))
と表現できる。
<i>else...</i> が省略されているかどうかは，マクロの rest 引数が nil
かどうかに等しいから，結局，if は次のように定義できる。

```Lisp
(defmacro if (test then &rest else)
  (cons cond (cons (list test then)
                   (cond (else (list (cons t else)))))))
```

展開の様子を確かめる。

```
> (lambda () (if a b))
(#<lambda> (0) (cond (a b)))
> (lambda () (if a b c))
(#<lambda> (0) (cond (a b) (t c)))
> (lambda () (if a b c d))
(#<lambda> (0) (cond (a b) (t c d)))
```

Emacs Lisp は，
条件が成立している限り式の評価を繰り返す (while _test body..._)
を用意している。これは次のように定義できる。

```Lisp
(defmacro while (test &rest body)
  (list (lambda (a b c)
          (setq a (lambda () (cond ((b) (c) (a)))))
          (a))
        nil
        (list lambda () test)
        (cons lambda (cons () body))))
```


このマクロでは while 式を 3 引数のラムダ式の関数適用として表現する。
３個の実引数のうち二つは，_test_ と _body..._ を繰り返し評価するために
無引数ラムダ式の中に展開して作った関数引数である。

残りの１個の実引数の値は無意味である
(したがって，なるべく無駄にならないように，ここでは nil を与えている)。
なぜなら，これに対応する変数 a は，ラムダ式内部で setq 
により，ただちに末尾再帰的な関数として再定義されるからである。
この再帰関数では _test_ を評価する関数適用 (b) の結果が非 nil である限り，
_body..._ を評価する関数適用 (c) を行い，
さらに自分自身を末尾呼出しする。

字句的スコープのおかげにより，名前の衝突は発生しない。例えば，

```
> (setq a 5)
5
> (while (/= a 0) (print a) (setq a (- a 1)))
5
4
3
2
1
nil
> 
```

let> 式もラムダ式の関数適用として表現できる。
let 式の変数並びを，仮引数のリストと実引数のリストに再編し，
仮引数のリストをそのままラムダ式の仮引数並びとし，
実引数のリストを関数適用の実引数並びとすればよい。

例えば，(let ((a 10) (b 20)) (+ a b)) は
((lambda (a b) (+ a b)) 10 20) と表現できる。
マクロ定義は次のようになる。

```Lisp
(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (if (null x) nil
         (cons (if (atom (car x)) (car x) (caar x))
               (vars (cdr x)))))
     (defun vals (x)
       (if (null x) nil
         (cons (if (atom (car x)) nil (cadr (car x)))
               (vals (cdr x)))))
     (cons (cons lambda (cons (vars args) body))
           (vals args)))
   nil nil))
```

ここでは仮引数リストと実引数リストを構築する補助的な関数として vars 
と vals を定義しているが，実は (while のマクロ定義の場合と異なり) 
これらをラムダ式の中にローカルにしなくてもよい。
vars と vals を大域関数として定義しても正しく機能する。
あえて上記のようにした理由は，あまり他に用途がない vars と vals 
を大域的な名前空間に入れないことで全体の繁雑さを抑えるためである。

いったん let を用意すれば，ローカルな関数を定義する方法はより簡単になる。
関数の名前を let 式の変数並びに連ねて，let 式の本体で defun すればよい。
相互再帰も可能である。
下記は，let と同じように機能するマクロ (名前は仮に my-let とする)
を let を使って作った例である。

```
> (defmacro my-let (args &rest body)
    (let (vars vals)
      (defun vars (x)
        (if (null x) nil
          (cons (if (atom (car x)) (car x) (caar x))
                (vars (cdr x)))))
      (defun vals (x)
        (if (null x) nil
          (cons (if (atom (car x)) nil (cadr (car x)))
                (vals (cdr x)))))
      (cons (cons lambda (cons (vars args) body))
            (vals args))))
my-let
> (my-let ((a 10) (b 20)) (+ a b))
30
> (lambda () (my-let ((a 10) (b 20)) (+ a b)))
(#<lambda> (0) ((lambda (a b) (+ a b)) 10 20))
```

よく知られているように let 式 (あるいはその正体であるラムダ式) は，
関数間にまたがる変数を隠すことにも使える。
例えば次の例では，let 式の変数 count は大域関数 next と reset 
だけからアクセスできる。

```
> (let ((count 0))
    (defun next () (setq count (+ count 1)))
    (defun reset () (setq count 0)))
reset
> (next)
1
> (next)
2
> (reset)
0
> (next)
1
> (next)
2
> (next)
3
```

ここで大域的に count を評価しても，大域変数としては未定義だから
[§1](#1) の 4. に基づき，本処理系ではシンボルそれ自身が得られるだけである。

```
> count
count
```

現在 3 が代入されているはずの変数は，next や reset の中に見ることができる。

```
> next
(#<lambda> (0 (3)) (setq (#<arg> 1 0 . count) (+ (#<arg> 1 0 . count) 1)))
> reset
(#<lambda> (0 (3)) (setq (#<arg> 1 0 . count) 0))
```

ここで (0 (3)) の cdr 値である ((3) . nil) が関数の字句的環境である。
変数値 3 は level 1 のフレームの最初 (第 0 番) の要素として格納されている。
詳しくは [§4](#4) を参照されたい。


<a name="7"></a>
## 7. 簡単なベンチマーク・テスト

処理系のおよその速度を調べるため，
フィボナッチ関数にかかる時間を起動時間込みで比較した。
実行環境は Pentium4 3.2GHz, 1GB RAM, Windows XP SP2, Cygwin 1.5.24-2
である。アンチウィルス等が動作しているため，結果はあくまで目安である。

L2Lisp 1.1 で [fib.l](../examples/fib.l) を実行
(使用した Cygwin 上の Pascal コンパイラとそのオプションは [§2](#2) のとおり):

```
$ time ./llsp < fib.l
> fib
> 832040
> Goodbye.

real    0m1.797s
user    0m1.686s
sys     0m0.108s
$ 
```

Cygwin 版 guile 1.8.1
で [fib.scm](../examples/fib.scm) を実行:

```
$ time guile < fib.scm
guile> guile> 832040
guile> 
real    0m2.031s
user    0m1.952s
sys     0m0.077s
$ 
```

java 1.6.0_01 上の
[jakld](http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/jakld/index-j.html)
で [fib.scm](../examples/fib.scm) を実行
(Cygwin 上のプロセスではないから real 以外の時間表示はほとんど意味がない。
実際，手もとの時計で測っても約 9 秒かかっている):

```
$ time java Eval < fib.scm
Java Application Kumikomi-you-no Lisp Driver (October 19, 2002)
(c) Copyright Taiichi Yuasa, 2002.  All rights reserved.

>fib

>832040

>
Sayonara

real    0m9.031s
user    0m0.031s
sys     0m0.015s
$ 
```


Cygwin 版 python 2.5.1
で [fib.py](../examples/fib.py) を実行:

```
$ time python fib.py
832040

real    0m1.266s
user    0m1.140s
sys     0m0.092s
$ 
```

この結果からは本処理系の速度は
インタープリタとしてそれほど悪くないと言ってよいだろう。
