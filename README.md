# РК2, Санталов Дмитрий ИУ9-51Б, Вариант 23

## Задача 1

SRS

a -> bab

ac -> $a^2$

ba -> ac

над множеством базисных слов $b^na^n$

Язык L = \{ $ {w | \exists  n: b^n a^n \rightarrow^* w}$ \} 

Из SRS имеем: $ba \rightarrow ac \rightarrow aa$
Пусть h(a) = a, h($\gamma, \gamma \neq a) = \varepsilon$.
Рассмотрим $\omega = b^na^n$. При n = 1: $\omega _1 = ba; ba \rightarrow aa = \omega_1'$. h($\omega_1') = a^2$.
Пусть верно для n, $\omega_n = b^na^n \rightarrow^* w_n'$, а $h(w_n') = a^{2^n}$.
Рассмотрим n+1: $\omega_{n+1} = bb^na^na \rightarrow b\omega_n'a = \omega_{n+1}'$.
Имеем в $\omega_n' \; 2^n$ букв a. Применив к ним правило $a \rightarrow bab$, получим следующую цепочку: $a \rightarrow bab \rightarrow aab$, т. е. мы удваиваем количество букв a $\Rightarrow$ получаем h($\omega_{n+1}'$) = $a^{2^{n+1}}$.
Рассмотрим $D = \{2^n | n \geq 1\} \subseteq \{|\omega|: \omega \in$ h$(L)\}$ - не полулинейно $\Rightarrow$ по теореме Париха h(L) - не КС $\Rightarrow$ L - не КС.

## Задача 2

$\{\omega \; | \; |\omega|_{ab} = |\omega|_{baa} \vee \omega = \omega^R \}$. Алфавит $\{a,b\}$
Возьмем R = $ab(a|b)^*ba$. Пусть $\omega \in R \Rightarrow |\omega|_{ab} = |\omega|_{ba}$; $|\omega|_{baa} = |\omega|_{ba} - |\omega|_{bab} - 1 \leq |\omega|_{ba} - 1 = |\omega|_{ab} - 1$; $|\omega|_{baa} < |\omega|_{ab}$
$\Rightarrow \{\omega: |\omega|_{ab} = |\omega|_{baa} \} \cap R = \varnothing \Rightarrow L \cap R = \{\omega = \omega^R\} \cap R$

$L \cap R = \{abuba \; | \; u = u^R \}$.
Пусть L - DCFL. Тогда $L \cap R$ - тоже DCFL, но язык палиндромов не является детерминированным $\Rightarrow$ L - не DCFL.

## Задача 3

$L(T) = (a|b)^*bb$, атрибут a считает число букв a в слове, $L(T) \subseteq L(S)$.
Предикат из $S \rightarrow SSS$ не исключает ни одного слова $\omega \in L(T)$. Кроме того, все выводы из S дают слова, оканчивающиеся на $bb$, то есть $L(S) \subseteq (a|b)^*bb$. Таким образом, $L(S) = (a|b)^*bb \Rightarrow$ язык КС и регулярный.

