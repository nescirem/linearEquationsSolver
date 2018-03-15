Slove Linear Equations
========================
###### _flowchart and program control flow_ ######

 _Values_    | Define
-------------|-----------------------------------------------------------------------------------------
 `A`         | Coefficient matrix
 `b`         | Right side vector
 `A\|b`       | Augmented matrix which contains coefficient matrix `A` and right side vector `b`
 `iter`      | Number of iterations
 `Err_limit` | Iterative error
 `Lax_factor`| Relaxation factor
-------------|-----------------------------------------------------------------------------------------
 `L`         | Lower triangular matrix of `A`
 `U`         | Upper triangular matrix of `A`
 `R`         | `R`=`L`+`U`
 `D`         | Diagonal matrix of `A`
 `M`         | `M`=invert`D`
 `P`         | Arrangement matrix
 `y`         | Intermediate vector in direct methods and jacobi method
 `solution`  | Repersent `x`, which defined by `A`·`x`=`b`

--------------------
## Direct methods ##

- [x]  [**Gaussian elimination**][Gaussian_elimination] method with [pivoting][]
- [x]  [**LU decomposition**][LU] method with [pivoting][]
- [ ]  [**TDMA**][TDMA] method with [pivoting][]

###### _control flow and flowcharts of_ **Gaussian elimination** ######
  * **Input:** `A|b`

###### _control flow and flowcharts of_ **LU decomposition** ######
  * **Input:** `A`,`b`
--------------------
## Iterative methods ##

- [x] [**Jacobi method**][Jacobi]
- [x] [**Gauss–Seidel method**][Gauss_Seidel]
- [x] [**Successive over-relaxation method**][SOR]


###### _control flow and flowcharts of_ **Jacobi method** ######
  * **Input:** `A`,`b`,`iter`,`Err_limit`  
###### _control flow and flowcharts of_ **Gauss–Seidel method** ######
  * **Input:** `A`,`b`,`iter`,`Err_limit`  
###### _control flow and flowcharts of_ **Successive over-relaxation method** ######
  * **Input:** `A`,`b`,`iter`,`Err_limit`,`Lax_factor`  

[Gaussian_elimination]:https://en.wikipedia.org/wiki/Gaussian_elimination "Refer to WIKIPEDIA."
[pivoting]:https://en.wikipedia.org/wiki/Pivot_element "Refer to WIKIPEDIA."
[LU]:https://en.wikipedia.org/wiki/LU_decomposition "Refer to WIKIPEDIA."
[TDMA]:https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm "Refer to WIKIPEDIA."

[Jacobi]:https://en.wikipedia.org/wiki/Jacobi_method "Refer to WIKIPEDIA."
[Gauss_Seidel]:https://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method "Refer to WIKIPEDIA."
[SOR]:https://en.wikipedia.org/wiki/Successive_over-relaxation "Refer to WIKIPEDIA."
