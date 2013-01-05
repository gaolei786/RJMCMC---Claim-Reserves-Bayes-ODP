RJMCMC 与准备金评估模型选择
===
---

我的主要工作就是用程序把RJMCMC实现。我的思路是这样的：


  1.  定义若干更新函数。我们记为<code>A1(),A2()....</code>    
  2.  定义总函数<code>move(n),n</code>为循环次数。在这个函数的主要结构为一个循环，这个循环依次将前面定义的更新函数串联起来。函数的返回结果是一个列表（更新的参数特别多，类型不一，最好的返回类型就是列表）。

另外，还专门定义了一个准备金广义线性模型求解的函数<code>mle()</code>.

需要引用的包是<code>LearBayes</code>,用到这个包中的<code>rwmetrop()</code>,即MH采样函数，完成指数递减曲线两个参数的采样。

对定义的几个函数给出说明：

1. mle()，在广义线性模型框架内求解准备金。
2. logpost(),move.alpha.beta(),共同完成指数递减曲线两个参数的更新。
3. move.u.r()，对事故年水平和进展年水平进行吉布斯采样，当然，只有在模型不发生转移的条件下才会调用这个函数。
4. birth(),death()给出模型转移的概率。
5. move()，可以直接运行的函数。其需要采样的初始值，采样次数等。其内又一循环结构，循环结构里分三种情况：第一种，模型不转移，调用move.alpha.beta与move.u.r();第二种，k+1,给出建议，判断；第三种，k-1,给出建议，判断。最后返回为一列表。

给出人工构造例子的相关结果。

![tupian](https://raw.github.com/gaolei786/RJMCMC---Claim-Reserves-Bayes-ODP/master/graph/1.png)

![tupian](https://raw.github.com/gaolei786/RJMCMC---Claim-Reserves-Bayes-ODP/master/graph/2.png)

![tupian](https://raw.github.com/gaolei786/RJMCMC---Claim-Reserves-Bayes-ODP/master/graph/3.png)

![tupian](https://raw.github.com/gaolei786/RJMCMC---Claim-Reserves-Bayes-ODP/master/graph/4.png)

![tupian](https://raw.github.com/gaolei786/RJMCMC---Claim-Reserves-Bayes-ODP/master/graph/5.png)


---

RJMCMC的代码储存于上面code文件夹中，数据存储于上面data文件夹中。
