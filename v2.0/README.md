# Little Lambda Lisp 2 in Standard Pascal

May 2007 (Edit: April 2018) by (鈴)

## v1.1 からの変更点

 1. L2Lisp.p の変更点
    * 入れ子のラムダ式のコンパイル
    * 入れ子マクロ定義の陽な禁止
    * 未インターン・シンボルの実現
    * make-symbol 関数を非組込みに

 2. l2init の変更点
    * make-symbol を定義
    * gentemp に替え gensym を定義

<a name="1"></a>
## 1. はじめに

L2Lisp (Little Lambda Lisp) [v1.1](../v1.1/README.md) は
[前回 §7](../v1.1/README.md#7) で示したとおり
[fib.l](../examples/fib.l)

```Lisp
(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 30)
```

のようなプログラムを実行するときは適度に高速である:

しかし，[10queens.l](../examples/10queens.l)
で 6 および 10 を引数にした N クイーン問題

```Lisp
(nqueens 6)  ; ((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
(length (nqueens 10))  ; 724
```

を解かせるとそれほど速くない。

その主な理由は v1.1 
が入れ子のラムダ式をその都度毎回 "コンパイル" するからである。

[前回 §6](../v1.1/README.md#6) で説明したように
L2Lisp は let や defun をラムダ式を含んだ式へのマクロとして実装している。
`nqueens` 関数の内部は入れ子のラムダ式を大量に含んだ S 式の並びになる。
しかし v 1.1 はラムダ式を評価するとき，最外のラムダ式だけをコンパイルする。
内側のラムダ式は必要になったとき，その都度コンパイルする。

そこで v2.0 では，ラムダ式を評価するとき，
入れ子のラムダ式を再帰的に一括してコンパイルするように改めた。
下記にその効果を例示する。

v1.1 の実行例を示す。
テスト環境は [前回 §7](../v1.1/README.md#7) と同様である。

```
$ time ./llsp11.exe < 10queens.l
> node-expand
> safe?
> safe-aux?
> goal?
> nqueens
> ((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
> 724
> Goodbye.

real    0m19.782s
user    0m19.702s
sys     0m0.046s
```

v2.0 の実行例を示す。
上記 v1.1 の結果に対し v2.0 
は下記のとおり起動時間込みで約 1/3 の時間で終了した。

```
$ time ./llsp.exe < 10queens.l
> node-expand
> safe?
> safe-aux?
> goal?
> nqueens
> ((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
> 724
> Goodbye.

real    0m6.406s
user    0m6.280s
sys     0m0.092s
```

この高速化を実現した v2.0 のラムダ式のコンパイルとマクロ展開の方法を 
[§2](#2) で説明する。

v2.0 は v1.1 の make-symbol 関数がインターン済みのシンボルを作ってしまう問題 
([前回 §5](../v1.1/README.md#5)) も改修した。

 - Paul Graham 著 野田 開 訳「On Lisp」オーム社刊 平成19年(2007年) 3月

の第９章 "変数捕捉" で説明されているように，
Lisp の伝統的なマクロ定義は "マクロ引数の捕捉" による名前の衝突を回避するため，
新しくその場で一意性のあるシンボルを作る必要がしばしばある。
しかし，ここでインターン済みのシンボルを作ると，
回収困難なかたちで資源を消費する。
v1.1 の実装では，やがてシンボル領域を使い尽くし，新しいシンボルを定義できなくなる。
v2.0 の未インターン・シンボルの実現方法を [§3](#3) で説明する。

2.0 版の処理系のコンパイルと実行の方法は [v1.1](../v1.1/README.md#2)
と同じである。


<a name="2"></a>
## 2. ラムダ式のコンパイルとマクロ展開

v1.1 でラムダ式を評価すると，
入れ子のラムダ式は，最外ラムダ式の仮引数の出現を除き，そのまま残される。
v1.1 による下記の例では，シンボル a の出現がレベル 0 オフセット 0 
のローカル変数参照へ変換されていることを除き，
入れ子のラムダ式は変更を受けていない。

```
> (lambda (a) (lambda (b) (foo a b)))
(#<lambda> (1) (lambda (b) (foo (#<arg> 0 0 . a) b)))
```

v1.1 でこのようなラムダ式で関数を定義すると，
入れ子のラムダ式が関数の実行ごとに使い捨てでコンパイルされるため非効率である。
一つの解決方法は，入れ子のラムダ式が評価される都度，
その結果を使い捨てにせずにメモすることである。
しかし v2.0 では，その方法はとらず，
ラムダ式の評価時に入れ子のラムダ式も一括して再帰的にコンパイルする方法をとった。
手続き EvalTop とその内部手続き CompileLambda ([前回 §4](../v1.1/README.md#4)
を参照) の局所的な改修だけで大半実現できるからである。

v2.0 による例を示す。

```
> (lambda (a) (lambda (b) (foo a b)))
(#<closure> (1) (#<lambda> 1 (foo (#<arg> 1 0 . a) (#<arg> 0 0 . b))))
```

つまり，典型的には，関数を定義したとき，そのコンパイルはすべて済んでいる。

```
> (defun bar (a) (lambda (b) (foo a b)))
bar
> bar
(#<closure> (1) (#<lambda> 1 (foo (#<arg> 1 0 . a) (#<arg> 0 0 . b))))
```


今までの方法では，ラムダ式をコンパイルするとき，仮引数の変換と字句的環境の付加の両方をおこなっていた。
しかし，最外ラムダ式を除き，コンパイル時に字句的環境はまだない。
そこで，コンパイル済みラムダ式を `#<lambda>` 
を先頭要素とする字句的環境なしの式と，
`#<closure>` を先頭要素とする字句的環境ありの式の２種類に分ける。

`#<lambda>` を先頭要素とする字句的環境なしの式は，仮引数並びにかえて，
期待される引数の個数 (arity) をおく
(上記の例では 1)。

`#<closure>` を先頭要素とする字句的環境ありの式は，仮引数並びにかえて，
arity と字句的環境のリストのペアをおく (上記の例では (1) つまり (1 . nil))。

手続き CompileLambda は，相互再帰的な内部手続き 
[Compile](L2Lisp.p#L973-L989)
と 
[CompileInners](L2Lisp.p#L991-L1014) を使って，
与えられたラムダ式を再帰的に字句的環境なしの式にコンパイルし，
最後に最外ラムダ式を，手続き 
[MakeClosure](L2Lisp.p#L871-L880) を使って
(その時点での処理系の大域変数 environ を字句的環境のリンクとして) 
字句的環境ありの式に替える。

手続き EvalTop は，字句的環境なしの式を評価する都度，それを手続き MakeClosure 
を使って字句的環境ありの式に替える。

手続き MakeClosure を下記に示す。
ValType, Push, Cons, cell については [前回 §3](../v1.1/README.md#3)
を参照されたい。
[EvalTop](L2Lisp.p#L1289)
は `#<closure>` を表す大域定数 ClosureImp 
を引数 newkar として MakeClosure 手続きを呼び出す。

```Pascal
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
```

ラムダ式をコンパイルするとき，それぞれのラムダ式本体に対してマクロ展開を行う。
展開は，コンパイル済みマクロ式，
またはコンパイル済みマクロ式に束縛された大域変数が，
関数の位置にあったときに行う。例を示す。

```
> if
(#<macro> (-3) (cons cond (cons (list (#<arg> 0 0 . test) (#<arg> 0 1 . then)) (
cond ((#<arg> 0 2 . else) (list (cons t (#<arg> 0 2 . else))))))))
> (lambda (a) (if a (lambda (b) (if b (foo a b)))))
(#<closure> (1) (cond ((#<arg> 0 0 . a) (#<lambda> 1 (cond ((#<arg> 0 0 . b) (fo
o (#<arg> 1 0 . a) (#<arg> 0 0 . b))))))))
```

内部手続き Compile は，ラムダ式の仮引数を __変換した後で__，
ラムダ式本体にあるマクロを手続き
[ExpandGlobalMacros](L2Lisp.p#L1181-L1216)
を使って展開する。
ExpandGlobalMacros はそれ自身では入れ子のラムダ式の中に立ち入らない。
入れ子のラムダ式に対し再帰的に Compile が呼び出されたとき，
そこから呼び出される ExpandGlobalMacros が入れ子のラムダ式本体をマクロ展開する。

この方法の帰結として「On Lisp」第９章で説明されている
**"自由なシンボルの捕捉"**
は発生しない。例えば，

```
> (defmacro m (n) (list 'setq 'w n))
m
> ((lambda (w) (m 3) (print w) ((lambda (w) (m 4) (print w)) 100)) 200)
200
100
100
> w
4
```

マクロ m が展開する変数 w への代入に，ラムダ式の仮引数 w は影響を受けない。
マクロによる変数 w への代入は大域的に行われる。
これはラムダ算法 (のアルファ変換) を考慮すれば妥当な振舞であろう。

実装上，なぜこうなるかはラムダ式の評価値から明らかである。

```
> (lambda (w) (m 3) (print w) ((lambda (w) (m 4) (print w)) 100))
(#<closure> (1) (setq w 3) (print (#<arg> 0 0 . w)) ((#<lambda> 1 (setq w 4) (pr
int (#<arg> 0 0 . w))) 100))
```

仮引数が __変換された後で__ マクロを展開しているから，あえて内部実装シンボル
`#<arg>` をじかに扱わない限り，捕捉は起こらない。

* L2Lisp v2.0 が現在のような仕様になっているのは，
  名前の由来でもあるラムダ算法との適合性を目指して…というより，
  実はむしろ，お手軽な実装で済ませたいためでした。
  仮引数を変換した後に残存しているのは自由なシンボルだけのはずですから，
  関数位置にあるシンボルのうち (`#<macro>` …) を値としているものを
  どんどん展開していけばよいわけです。

一方，Common Lisp では自由なシンボルの捕捉が発生する。
Cygwin 上の 
[clisp](http://clisp.cons.org) の実行例を示す。
[1] のマクロ m における自由なシンボル (展開結果に自由変数として出現するシンボル) 
である w が，[2] 
のラムダ式適用でラムダ式の同名の仮引数によって意図せず捕捉されている。
この弱点については Emacs Lisp も Common Lisp と基本的に同様である。

```
[1]> (defmacro m (n) (list 'setq 'w n))
M
[2]> ((lambda (w) (m 3) (print w) ((lambda (w) (m 4) (print w)) 100)) 200)

3
4
4
[3]> w

*** -EVAL: variable W has no value
```

* [2] の式は１行で書くには少し複雑ですが，分かりやすく字下げするとこうなります:
  ```Lisp
  ((lambda (w)
     (m 3)
     (print w) 
     ((lambda (w)
        (m 4)
        (print w)) 
      100))
   200)
  ```



可能性としてはマクロ式もラムダ式と同じく字句的環境をもてるが，
v2.0 では簡単のため，大域的な (つまり空の字句的環境をもつ) マクロ式だけを許す。
展開対象のラムダ式と同じ字句的環境を共有している場合，
ラムダ式のコンパイル時には，まだその環境はない。
さしあたり今は非大域的なマクロ式を一律に禁止して微妙な問題の発生を避ける。
生のマクロ式 (macro …) からコンパイル済みマクロ式 (`#<macro>` …) への変換は
(ラムダ式の場合とほとんどの処理が共通だから) 手続き CompileLambda が行う。
CompileLambda は，内部手続き CompileInners 
で入れ子のマクロ式のコンパイルをエラー扱い 
[(goto でエラー処理へ大域脱出)](L2Lisp.p#L1001) するとともに，
最外のマクロ式に対し
environ が nil 値であることを
[チェック](L2Lisp.p#L1022)して大域的であることを保証する。

ローカル変数が大域的なコンパイル済みマクロ式に束縛されている場合，コンパイル時には展開されず，
適用時に展開と評価が行われる。
処理系の大域定数 
[MaxMacroExps](L2Lisp.p#L36)
で規定される限度を超えて再帰的に展開しようとするマクロ式も，
限度超過分は，適用時に展開と評価が行われる。
前者の例を示す。

```
> (defmacro m (e) (list 'progn (list 'print e) (list 'print e) nil))
m
> (m "hi")
"hi"
"hi"
nil
> (defun f (x) (x (progn (prin1 "hi") "ho")))
f
> f
(#<closure> (1) ((#<arg> 0 0 . x) (cond (t (prin1 "hi") "ho"))))
> (f m)
"hi""ho"
"hi""ho"
nil
>
```


<a name="3"></a>
## 3. 未インターン・シンボルの実現

前節で述べたとおり L2Lisp では自由なシンボルの捕捉は発生しない。
しかし「On Lisp」第９章で述べられているもう一種類の変数捕捉である
**"マクロ引数の捕捉"** は L2Lisp でも発生する。
次の例は，大域変数であれローカル変数であれ，シンボル a 
を引数に渡したときだけ，変数捕捉による奇妙な振舞を示す。

```
> (defmacro m (x)
    (list 'let '((a 4))
          (list 'if (list '< 'a x) (list 'print x))))
m
> (m 10)
10
10
> (m 1)
nil
> (setq a 10)
10
> (m a)
nil
> (let ((a 10)) (m a))
nil
> (let ((b 10)) (m b))
10
10
```

ダミーのラムダ式に入れてコンパイル結果を確認すると，
マクロ引数として与えたシンボル a
が，マクロ展開されるラムダ式の仮引数 a と混同されていることが分かる。

```
> (lambda () (m a))
(#<closure> (0) ((#<lambda> 1 (cond ((< (#<arg> 0 0 . a) (#<arg> 0 0 . a)) (prin
t (#<arg> 0 0 . a))))) 4))
> (lambda () (let ((a 10)) (m a)))
(#<closure> (0) ((#<lambda> 1 ((#<lambda> 1 (cond ((< (#<arg> 0 0 . a) (#<arg> 0
 0 . a)) (print (#<arg> 0 0 . a))))) 4)) 10))
```

この問題に対する Common Lisp 
等での標準の解決方法は未インターン・シンボルの動的生成である。
しかし，L2Lisp は [前回 §3](../v1.1/README.md#3) で示したように
シンボルをシンボル表 symbol の添え字の値として表現しているから，
本来，未インターン・シンボルは表現できない。
シンボルであることと，シンボル表に登録されていること 
(＝インターンされていること) が等価だからである。

そこで v2.0 では新しくデータ型を作るのではなく，
先頭がシンボル `*symbol*`，次が **文字列**，その次が任意の値である
長さ３の **リスト** をシンボル扱いすることにより，
未インターン・シンボルを表現できるようにした。
リストの第３要素をシンボルの **大域変数としての値** 
とし，処理系の次の箇所でシンボルとして扱う。

 1. 組込み関数 atom で (手続き [ApplySymbol](L2Lisp.p#L757))
 2. ラムダ式の仮引数として
    (手続き [Scan](L2Lisp.p#L899), 関数 [ArgList](L2Lisp.p#L955))
 3. スペシャルフォーム setq で代入される大域変数として
    (手続き [EvalSetqBody](L2Lisp.p#L1156))
 4. 評価される大域変数として (手続き [EvalTop](L2Lisp.p#L1294))

このとき，文字列 s から未インターン・シンボルを作る関数を 
[l2init](l2init)
で次のように定義できる。

```Lisp
(defun make-symbol (s) (list *symbol* s nil))
```

ここで `*symbol*` は自分自身へと評価されるから，クォートは不要である。
list の第３引数の値 nil は make-symbol が作る未インターン・シンボル
の大域変数としての初期値となる。

使用例を示す。

```
> (setq a (make-symbol "poi"))
(*symbol* "poi" nil)
> a
(*symbol* "poi" nil)
> (eval (list 'setq a 4))
4
> a
(*symbol* "poi" 4)
```

未インターン・シンボルを次々と作る gensym 関数は次のように書ける。
48 は '0' に対する ASCII コードである。
L2Lisp の文字列の表現方法については
[前回 §1](../v1.1/README.md#1) の 12. を参照されたい。

```Lisp
(let ((counter 0))
  (defun gensym ()
    (setq counter (+ counter 1))
    (let ((c counter) d4 d3 d2 d1)
      (setq d4 (/ c 1000) c (- c (* d4 1000))
            d3 (/ c 100)  c (- c (* d3 100))
            d2 (/ c 10)   c (- c (* d2 10))
            d1 c)
      (make-symbol
       (append "G" (mapcar (lambda (d) (+ d 48)) (list d4 d3 d2 d1)))))))
```

使用例を示す。

```
> (gensym)
(*symbol* "G0001" nil)
> (gensym)
(*symbol* "G0002" nil)
> (gensym)
(*symbol* "G0003" nil)
```

これにより，[前回 §6](../v1.1/README.md#6) の while マクロは，
実行時のラムダ式の入れ子を減らして，より効率的に実現できる
(ちなみに，どちらも while によるループは末尾再帰として最適化される)。


```Lisp
(defmacro while (test &rest body)
  (let ((a (gensym)))
    (list let (list a)
          (list setq a
                (list lambda ()
                      (list cond (cons test (append body (list (list a)))))))
          (list a))))
```

ここではマクロ展開で作るラムダ式の仮引数を (gensym) 
で生成した新しい未インターン・シンボルで表しているから，
**"マクロ引数の捕捉"**
は発生しない。

```
> (let ((a 0))
    (while (< a 5)
       (print a)
       (setq a (+ a 1))))
0
1
2
3
4
nil
```


<a name="4"></a> 
## 4. おわりに

L2Lisp v1.1 にあった弱点の比較的小規模な改修による解決を述べた。
標準 Pascal によるプログラミングは，
特定の言語の機能に依存しすぎないデータ構造と処理方法を表すためには妥当だが，
コマンド行引数を直接扱えないなど，そのままの実用化には困難がある。
この実装をひながたとして，さまざまな言語への移植を試みることは興味深い課題であろう。

