## Vector Dot-Product (Inner-Product)

- Input: 2 vectors (dimension N)
- Output: 1 scalar

$$
s=\mathbf{a}\cdot{}\mathbf{b}\implies
s=\sum_{i=1}^N{a_{i}b_i}
$$

```fortran
s = 0
do i = 1,N
  s = s + a(i)*b(i)
endDo
```
More explicit, let **a** and **b** be 3-elements long. Then, their dot-product, _s_, is
$$
s = \sum_{i=1}^3{a_i b_i} = a_1 b_1 + a_2 b_2 + a_3 b_3
$$
Alternatively, we could let
$$
s_1 = a_1 b_1\,\,\,\,;\,\,\,\,s_2=a_2 b_2\,\,\,\,;\,\,\,\,s_3=a_3 b_3
$$
and then,
$$
s = \sum_{i=1}^3{a_i b_i} = s_1 + s_2 + s_3
$$

## Matrix-Vector Product

- Input: 1 matrix (N x M), 1 vector (M)
- Output: 1 vector (N)

$$
\mathbf{b} = \mathbf{Ax}
\implies
b_{i}=\sum_{j=1}^N{A_{ij}x_j}
$$

```fortran
real,dimension(N)::vecB
real,dimension(N,M)::matA
real,dimension(M)::vecX
---
vecB = 0
do i = 1,N
  do j = 1,M
    vecB(i) = vecB(i) + matA(i,j)*vecX(j)
  endDo
endDo
```



## Matrix-Matrix Product

- Input: 2 matrices (one is M x N and one is N x P)
- Output: 1 matrix (M x P)

$$
\mathbf{C}=\mathbf{AB}
\implies
C_{ij}=\sum_{k=1}^N{A_{ik}B_{kj}}
$$

```fortran
real,dimension(N,M)::matC
real,dimension(N,P)::matA
real,dimension(P,M)::matB
---
matC = 0
do i = 1,N
  do j = 1,M
    do k = 1,P
      matC(i,j) = matC(i,j) + matA(i,k)*matB(k,j)
    endDo
  endDo
endDo
```
