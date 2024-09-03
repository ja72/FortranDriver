# FortranDriver

## Fortran Library `FotranDriverDLL`

Compiled with IFX and exports functions that are mostly wrappers to Fortran functions. 

For example, below is the code that wraps the Fortran native matrix multiplication intrinsic.

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
