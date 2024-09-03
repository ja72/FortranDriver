# FortranDriver

## Fortran Library `FotranDriverDLL`

Compiled with IFX and exports functions that are mostly wrappers to Fortran functions. 

For example, below is the code that wraps the Fortran native matrix multiplication intrinsic.

```fortran
subroutine array_product_mm(n,m,k,A,x,b)
!DEC$ ATTRIBUTES DLLEXPORT :: array_product_mm
!DEC$ ATTRIBUTES ALIAS: 'array_product_mm' :: array_product_mm
!DEC$ ATTRIBUTES VALUE :: n, m, k
!DEC$ ATTRIBUTES REFERENCE :: A, b, x
integer, intent(in) :: n,m,k
real(real64), intent(in) :: A(n,m), x(m,k)
real(real64), intent(out) :: b(n,k)
    
    b = matmul(A, x)    
            
end subroutine
```

For matrix inversion, solution of system of equations and for determinant evaluation
the Fortran library does a direct evaluation for systems with
`n <= 4` or otherwise it uses a `LUP` decomposition for higher
dimensional systems.

Some of the exported functions are

  - `array_rand_v(integer, real, real, real(:))`
  - `array_elem_v(integer, integer, real, real(:))`
  - `array_add_v(integer, real(:), real(:), real(:))`
  - `array_subtract_v(integer, real(:), real(:), real(:))`
  - `array_scale_v(integer, real, real(:), real(:))`
  - `array_dot_v(integer, real(:), real(:), real)`
  - `array_product_mv(integer, integer, real(:,:), real(:), real(:))`
  - `array_product_vm(integer, integer, real(:), real(:,:), real(:))`
  - `array_solve_mv(integer, integer, real(:,:), real(:), real(:))`
  - `array_rand_m(integer, integer, real, real, real(:,:))`
  - `array_diag_m(integer, real(:), real(:,:))`
  - `array_scalar_m(integer, integer, real, real(:,:))`
  - `array_add_m(integer, integer, real(:,:), real(:,:), real(:,:))`
  - `array_subtract_m(integer, integer, real(:,:), real(:,:), real(:,:))`
  - `array_scale_m(integer, integer, real, real(:,:), real(:,:))`
  - `array_det_m(integer, real(:,:), real)`
  - `array_tansp_m(integer, integer, real(:,:), real(:,:))`
  - `array_product_mm(integer, integer, integer, real(:,:), real(:,:), real(:,:))`
  - `array_solve_mm(integer, integer, integer, real(:,:), real(:,:), real(:,:))`
  - `array_inverse_m(integer, real(:,:), real(:,:))`

## C# Console App `FortranDriverCS`

 The corresponding imported functions are

 - `array_rand_v(int, double, double, double[]);`
 - `array_elem_v(int, int, double, double[]);`
 - `array_add_v(int, double[], double[], double[]);`
 - `array_subtract_v(int, double[], double[], double[]);`
 - `array_scale_v(int, double, double[], double[]);`
 - `array_dot_v(int, double[], double[], out double);`
 - `array_product_mv(int, int, double[,], double[], double[]);`
 - `array_product_vm(int, int, double[], double[,], double[]);`
 - `array_solve_mv(int, int, double[,], double[], double[]);`
 - `array_rand_m(int, int, double, double, double[,]);`
 - `array_diag_m(int, double[], double[,]);`
 - `array_scalar_m(int, int, double, double[,]);`
 - `array_add_m(int, int, double[,], double[,], double[,]);`
 - `array_subtract_m(int, int, double[,], double[,], double[,]);`
 - `array_scale_m(int, int, double, double[,], double[,]);`
 - `array_det_m(int, double[,], out double);`
 - `array_tansp_m(int, int, double[,], double[,]);`
 - `array_product_mm(int, int, int, double[,], double[,], double[,]);`
 - `array_solve_mm(int, int, int, double[,], double[,], double[,]);`
 - `array_inverse_m(int, double[,], double[,]);`

### `NativeVector` and `NativeMatrix`

All of the above imports are neatly wrapped inside a vector and matrix classes **that store the values in a way that is compatible with Fortran (column-major arrays)** and defines corresponding methods and operators.

#### `NativeVectors`

 - `NativeVector Elemental(int, int, double)`
 - `NativeVector Random(int, double, double)`
 - `double Dot(NativeVector, NativeVector)`
 - `NativeVector Negate(NativeVector)`
 - `NativeVector Add(NativeVector, NativeVector)`
 - `NativeVector Subtract(NativeVector, NativeVector)`
 - `NativeVector Scale(double, NativeVector)`
 - `NativeVector Product(NativeVector, NativeMatrix)`
 - `NativeVector op_UnaryPlus(NativeVector)`
 - `NativeVector op_Addition(NativeVector, NativeVector)`
 - `NativeVector op_UnaryNegation(NativeVector)`
 - `NativeVector op_Subtraction(NativeVector, NativeVector)`
 - `NativeVector op_Multiply(double, NativeVector)`
 - `NativeVector op_Multiply(NativeVector, double)`
 - `NativeVector op_Division(NativeVector, double)`
 - `double op_Multiply(NativeVector, NativeVector)`
 - `NativeVector op_Multiply(NativeVector, NativeMatrix)`

#### `NativeMatrix`

 - `NativeMatrix Random(int, int, double, double)`
 - `NativeMatrix Zero(int)`
 - `NativeMatrix Zero(int, int)`
 - `NativeMatrix Identity(int)`
 - `NativeMatrix Identity(int, int)`
 - `NativeMatrix Scalar(int, double)`
 - `NativeMatrix Scalar(int, int, double)`
 - `NativeMatrix Diagonal(double[])`
 - `NativeMatrix Diagonal(int, double[])`
 - `NativeMatrix Transpose(NativeMatrix)`
 - `NativeMatrix Negate(NativeMatrix)`
 - `NativeMatrix Add(double, NativeMatrix)`
 - `NativeMatrix Add(NativeMatrix, double)`
 - `NativeMatrix Add(NativeMatrix, NativeMatrix)`
 - `NativeMatrix Subtract(double, NativeMatrix)`
 - `NativeMatrix Subtract(NativeMatrix, double)`
 - `NativeMatrix Subtract(NativeMatrix, NativeMatrix)`
 - `NativeMatrix Scale(double, NativeMatrix)`
 - `NativeVector Product(NativeMatrix, NativeVector)`
 - `NativeMatrix Product(NativeMatrix, NativeMatrix)`
 - `NativeVector Solve(NativeMatrix, NativeVector)`
 - `NativeMatrix Solve(NativeMatrix, NativeMatrix)`
 - `double Determinant(NativeMatrix)`
 - `NativeMatrix Inverse(NativeMatrix)`
 - `NativeMatrix op_UnaryPlus(NativeMatrix)`
 - `NativeMatrix op_UnaryNegation(NativeMatrix)`
 - `NativeMatrix op_Addition(double, NativeMatrix)`
 - `NativeMatrix op_Subtraction(double, NativeMatrix)`
 - `NativeMatrix op_Addition(NativeMatrix, double)`
 - `NativeMatrix op_Subtraction(NativeMatrix, double)`
 - `NativeMatrix op_Addition(NativeMatrix, NativeMatrix)`
 - `NativeMatrix op_Subtraction(NativeMatrix, NativeMatrix)`
 - `NativeMatrix op_Multiply(double, NativeMatrix)`
 - `NativeMatrix op_Multiply(NativeMatrix, double)`
 - `NativeVector op_Multiply(NativeMatrix, NativeVector)`
 - `NativeMatrix op_Multiply(NativeMatrix, NativeMatrix)`
 - `NativeMatrix op_Division(NativeMatrix, double)`
 - `NativeMatrix op_OnesComplement(NativeMatrix)`
 - `NativeVector op_Division(NativeVector, NativeMatrix)`
 - `NativeMatrix op_Division(NativeMatrix, NativeMatrix)`
 - `NativeMatrix op_LogicalNot(NativeMatrix)`

 ## Demo/Test code outout

 Verbatim the output from the C# test code that does some very basic linear 
 algebra by calling the corresponding method, which in term calls the 
 corresponding Fortran function. 

 Note that outputs have been rounded to 12 digits and then converted into text 
 using `g7` formatting.


 ```text
 Calling Fortran from C#


1. Generate Matrix A in C#
A=
|      1      2      3 |
|      4      5      6 |
|      7      8      9 |
|     10     11     12 |
|     13     14     15 |
|     16     17     18 |
|     19     20     21 |

 Step:            1 of  3
 Step:            2 of  3
 Step:            3 of  3
2. Generate Vector x in C#
x=
|    0.5 |
|      1 |
|    1.5 |

3. Manipulate Matrix A in Fortran
A=
|  1.581  4.845   9.55 |
|  4.863  10.56  18.26 |
|  7.779   16.7  27.98 |
|  10.42  22.92  36.12 |
|  13.89  28.02  45.36 |
|  16.75  34.73  54.77 |
|  19.37  40.75  63.34 |

4. Calculate Vector b in Fortran
b=
|  19.96 |
|  40.39 |
|  62.57 |
|  82.31 |
|    103 |
|  125.3 |
|  145.4 |

5. Calculate Vector x in Fortran
g=
|    0.5 |
|      1 |
|    1.5 |
|      0 |
|      0 |
|      0 |
|      0 |


Testing Native/Fortran Vectors & Matrices
Vector Size =7
Elemental
e =
|           1 |
|           0 |
|           0 |
|           0 |
|           0 |
|           0 |
|           0 |


Random
x =
|     3.53389 |
|  -0.0474646 |
|  -0.0544103 |
|     4.88769 |
|    0.214773 |
|      4.4543 |
|    0.808256 |


Scaling/Negate
n =
|    -3.53389 |
|   0.0474646 |
|   0.0544103 |
|    -4.88769 |
|   -0.214773 |
|     -4.4543 |
|   -0.808256 |


Addition
y =
|     4.53389 |
|  -0.0474646 |
|  -0.0544103 |
|     4.88769 |
|    0.214773 |
|      4.4543 |
|    0.808256 |


Subtraction
z =
|    -2.53389 |
|   0.0474646 |
|   0.0544103 |
|    -4.88769 |
|   -0.214773 |
|     -4.4543 |
|   -0.808256 |


Dot Product
d = -55.92335217964191
Random
A =
|    -0.62464     5.28075     3.70127     2.73815     5.18095     4.92262     1.97883 |
|   -0.716179     2.25109     4.22183     4.06758    0.127354     4.29646   -0.457891 |
|   0.0702138     4.09558     2.90592     2.95752     5.83689   -0.332198     3.72654 |
|     2.20921     4.85445     2.05148     4.04802     2.07399     4.05893     3.54185 |
|     1.95034   -0.731776     1.45102     4.94672   0.0208383     3.75302    0.327344 |
|     3.03146     3.98451     4.83298     2.24488     1.68265     1.87458     4.80515 |
|     1.34869     1.48229   -0.327615     1.00801     3.70934     4.66621     5.05912 |


Matrix/Vector Product
g =
|     35.3627 |
|     35.8086 |
|      17.137 |
|     48.6384 |
|     48.0123 |
|     33.8282 |
|     35.3108 |


Vector/Matrix Product
f =
|     23.6327 |
|     60.8481 |
|     44.3228 |
|     40.9843 |
|     38.6199 |
|     49.9764 |
|     49.6864 |


Matrix/Vector Solve
u =
|     3.53389 |
|  -0.0474646 |
|  -0.0544103 |
|     4.88769 |
|    0.214773 |
|      4.4543 |
|    0.808256 |


Residual
u-x =
|          -0 |
|           0 |
|          -0 |
|           0 |
|          -0 |
|          -0 |
|           0 |


Scalar
S =
|           2           0           0           0           0           0           0 |
|           0           2           0           0           0           0           0 |
|           0           0           2           0           0           0           0 |
|           0           0           0           2           0           0           0 |
|           0           0           0           0           2           0           0 |
|           0           0           0           0           0           2           0 |
|           0           0           0           0           0           0           2 |


Diagonal
D =
|     2.49788           0           0           0           0           0           0 |
|           0     5.42568           0           0           0           0           0 |
|           0           0     5.95427           0           0           0           0 |
|           0           0           0     5.75542           0           0           0 |
|           0           0           0           0   -0.750938           0           0 |
|           0           0           0           0           0     1.96877           0 |
|           0           0           0           0           0           0     2.51319 |


Random
R =
|     4.50354      5.3711     3.54005      5.2476    0.755623     2.32017     1.05255 |
|     4.91874     2.77253     1.00319   -0.588027     4.76002   -0.938255     3.62032 |
|   -0.128475      5.5531     3.72581   -0.908082  -0.0738401     3.72192     4.80324 |
|     3.19735     2.15568     4.19711     1.27234     5.56115   -0.729062     3.71758 |
|     5.53569   -0.156161     2.81046     5.80818     1.62254     4.12428     5.38262 |
|     4.89418     2.24512     2.69301     4.40465     4.62534      2.9859     5.95033 |
|     2.37442     5.84556     3.71649     1.64767     4.02671     0.50351   -0.677844 |


Add
A =
|     7.00142      5.3711     3.54005      5.2476    0.755623     2.32017     1.05255 |
|     4.91874     8.19822     1.00319   -0.588027     4.76002   -0.938255     3.62032 |
|   -0.128475      5.5531     9.68009   -0.908082  -0.0738401     3.72192     4.80324 |
|     3.19735     2.15568     4.19711     7.02776     5.56115   -0.729062     3.71758 |
|     5.53569   -0.156161     2.81046     5.80818    0.871606     4.12428     5.38262 |
|     4.89418     2.24512     2.69301     4.40465     4.62534     4.95467     5.95033 |
|     2.37442     5.84556     3.71649     1.64767     4.02671     0.50351     1.83534 |


Subtract
B =
|     2.00567      5.3711     3.54005      5.2476    0.755623     2.32017     1.05255 |
|     4.91874    -2.65315     1.00319   -0.588027     4.76002   -0.938255     3.62032 |
|   -0.128475      5.5531    -2.22846   -0.908082  -0.0738401     3.72192     4.80324 |
|     3.19735     2.15568     4.19711    -4.48308     5.56115   -0.729062     3.71758 |
|     5.53569   -0.156161     2.81046     5.80818     2.37348     4.12428     5.38262 |
|     4.89418     2.24512     2.69301     4.40465     4.62534     1.01713     5.95033 |
|     2.37442     5.84556     3.71649     1.64767     4.02671     0.50351    -3.19103 |


Scale/Negate
N =
|    -4.50354     -5.3711    -3.54005     -5.2476   -0.755623    -2.32017    -1.05255 |
|    -4.91874    -2.77253    -1.00319    0.588027    -4.76002    0.938255    -3.62032 |
|    0.128475     -5.5531    -3.72581    0.908082   0.0738401    -3.72192    -4.80324 |
|    -3.19735    -2.15568    -4.19711    -1.27234    -5.56115    0.729062    -3.71758 |
|    -5.53569    0.156161    -2.81046    -5.80818    -1.62254    -4.12428    -5.38262 |
|    -4.89418    -2.24512    -2.69301    -4.40465    -4.62534     -2.9859    -5.95033 |
|    -2.37442    -5.84556    -3.71649    -1.64767    -4.02671    -0.50351    0.677844 |


Determinant
d =
-36200.84146176189
Transpose
A_tr =
|     7.00142     4.91874   -0.128475     3.19735     5.53569     4.89418     2.37442 |
|      5.3711     8.19822      5.5531     2.15568   -0.156161     2.24512     5.84556 |
|     3.54005     1.00319     9.68009     4.19711     2.81046     2.69301     3.71649 |
|      5.2476   -0.588027   -0.908082     7.02776     5.80818     4.40465     1.64767 |
|    0.755623     4.76002  -0.0738401     5.56115    0.871606     4.62534     4.02671 |
|     2.32017   -0.938255     3.72192   -0.729062     4.12428     4.95467     0.50351 |
|     1.05255     3.62032     4.80324     3.71758     5.38262     5.95033     1.83534 |


Inverse
A_inv =
|     3.93089     1.63875     2.10649     3.35991    -7.63678     4.87952    -11.2284 |
|    -3.37382    -1.35507    -1.87737    -3.03048     6.77471    -4.36059     9.92826 |
|      2.9805     1.15585     1.70668     2.64829    -5.90924     3.71788    -8.54332 |
|     -3.2229    -1.39087    -1.82862    -2.78981     6.51885    -4.17837     9.45685 |
|     2.17508    0.851885     1.17326      1.9683    -4.54762     2.97656     -6.2984 |
|    -0.64506   -0.406987   -0.395507   -0.738435     1.28146   -0.664938     2.10114 |
|    -2.07703   -0.653442    -1.02572    -1.66869     4.04198    -2.54981     5.50194 |


Check Identity
A_inv*A=
|           1          -0           0           0           0           0           0 |
|          -0           1           0          -0           0          -0          -0 |
|           0          -0           1           0          -0           0           0 |
|          -0          -0          -0           1          -0          -0          -0 |
|           0          -0           0           0           1           0           0 |
|          -0           0          -0          -0           0           1          -0 |
|           0           0          -0          -0           0           0           1 |


A_inv*A-1=
|           0          -0           0           0           0           0           0 |
|          -0           0           0          -0           0          -0          -0 |
|           0          -0           0           0          -0           0           0 |
|          -0          -0          -0          -0          -0          -0          -0 |
|           0          -0           0           0           0           0           0 |
|          -0           0          -0          -0           0          -0          -0 |
|           0           0          -0          -0           0           0          -0 |


Random
Y =
|     5.32029   -0.362404    0.670648 |
|   -0.739317      5.6228   -0.286091 |
|     2.11156   -0.737348     3.17619 |
|   -0.289686   -0.561174    0.598009 |
|     1.71312     2.95986     4.40034 |
|     2.62383     2.68891     5.94585 |
|   -0.918487     2.16345     5.07847 |


Product G=A*Y
G=
|      45.649     32.8607     40.0066 |
|     24.7641     63.3031     37.5406 |
|     21.1416     44.8235     74.7261 |
|     26.4431     26.4663     58.0768 |
|     41.1895     17.0988     71.8502 |
|     44.2477     46.2791     93.8589 |
|     22.2148      45.586      42.743 |


Solve A*U=G
U=
|     5.32029   -0.362404    0.670648 |
|   -0.739317      5.6228   -0.286091 |
|     2.11156   -0.737348     3.17619 |
|   -0.289686   -0.561174    0.598009 |
|     1.71312     2.95986     4.40034 |
|     2.62383     2.68891     5.94585 |
|   -0.918487     2.16345     5.07847 |


Residual
U-Y=
|           0           0           0 |
|          -0          -0          -0 |
|           0           0           0 |
|          -0          -0          -0 |
|           0           0           0 |
|          -0          -0          -0 |
|          -0          -0          -0 |
```

