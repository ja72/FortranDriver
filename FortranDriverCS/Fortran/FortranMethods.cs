using System.Drawing.Drawing2D;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using static System.Runtime.InteropServices.JavaScript.JSType;
using static JA.Program;

namespace JA.Fortran
{
    internal static partial class FortranMethods
    {
        // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
        //       delcaration instead. Use a ref to first element instead of passing a 2D array.
#if DEBUG
        public const string libraryName = "FortranDriverDLL_d";
#else
        public const string libraryName = "FortranDriverDLL";
#endif

        #region Interop Methods
        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [DllImport(libraryName, EntryPoint = "call_test_dowork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern void call_test_dowork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        #endregion

        #region Vector Functions
        /*
                        call_add_array_m
                        call_add_array_v
                        call_lu_array_block_solve_mm
                        call_solve_block_mv
                        call_determinant_array_m
                        call_array_diag_m
                        call_inner_array_v
                        call_elem_array_v
                        call_fill_array_m
                        array_inverse_m
                        call_norm_array_m
                        call_norm_array_v
                        call_mul_array_mm
                        call_mul_array_mv
                        call_mul_array_vm
                        call_random_array_m
                        call_random_array_v
                        call_reshape_array_mm
                        call_reshape_array_mv
                        call_reshape_array_vm
                        call_round_array_m
                        call_round_array_v
                        call_array_scalar_m
                        call_scale_array_m
                        call_scale_array_v
                        call_slice_array_m
                        call_slice_array_v
                        call_solve_array_mm
                        call_solve_array_mv
                        call_sub_array_m
                        call_sub_array_v
                        call_transpose_array_m
                        call_uniform_array_m
                        call_uniform_array_v
                //      outer_array_v  
                */

        [DllImport(libraryName, EntryPoint = "call_round_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_round_array_v(int size, [In] double[] x, int digits, [Out] double[] r);
        [DllImport(libraryName, EntryPoint = "call_round_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_round_array_m(int rows, int columns, [In] double[,] x, int digits, [Out] double[,] r);

        [DllImport(libraryName, EntryPoint = "call_norm_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_norm_array_v(int size, [In] double[] A, ref double s);

        [DllImport(libraryName, EntryPoint = "call_slice_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_array_v(int size, [In] double[] A, int start_index, int end_index, [Out] double[] B);

        [DllImport(libraryName, EntryPoint = "call_random_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_random_array_v(int size, [In] double minValue, [In] double maxValue, [Out] double[] A);
        [DllImport(libraryName, EntryPoint = "call_uniform_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_uniform_array_v(int size, ref int seed, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "call_elem_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_elem_array_v(int size, int index, [In] double x, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "call_add_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_add_array_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "call_sub_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_sub_array_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "call_scale_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_scale_array_v(int size, double x, [In] double[] y, [Out] double[] z);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="x">The first vector.</param>
        /// <param name="y">The second vector.</param>
        /// <param name="z">The dot product result.</param>
        [DllImport(libraryName, EntryPoint = "call_inner_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_inner_array_v(int size, [In] double[] x, [In] double[] y, [Out] out double z);

        #endregion

        #region Matrix Functions
        [DllImport(libraryName, EntryPoint = "call_fill_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_fill_array_m(int rows, int columns, double[] values, ElementOrder order, [Out] double[,] data);

        [DllImport(libraryName, EntryPoint = "call_random_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_random_array_m(int rows, int columns, [In] double minValue, [In] double maxValue, [Out] double[,] A);
        [DllImport(libraryName, EntryPoint = "call_uniform_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_uniform_array_m(int rows, int columns, ref int seed, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_array_diag_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_diag_m(int size, [In] double[] x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_array_scalar_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_scalar_m(int rows, int columns, [In] double x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_norm_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_norm_array_m(int rows, int columns, [In] double[,] A, ref double s);

        [DllImport(libraryName, EntryPoint = "call_add_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_add_array_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_sub_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_sub_array_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_scale_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_scale_array_m(int rows, int columns, double x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_determinant_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_determinant_array_m(int size, [In] double[,] A, [Out] out double det);

        [DllImport(libraryName, EntryPoint = "call_transpose_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_transpose_array_m(int rows, int columns, [In] double[,] A, [Out] double[,] At);

        [DllImport(libraryName, EntryPoint = "call_reshape_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_mv(int rows, int columns, [In] double[,] A, int new_size, [Out] double[] B);

        [DllImport(libraryName, EntryPoint = "call_reshape_array_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_vm(int size, [In] double[] A, int new_rows, int new_columns, ElementOrder order, [Out] double[,] B);

        [DllImport(libraryName, EntryPoint = "call_reshape_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_mm(int rows, int columns, [In] double[,] A, int new_rows, int new_columns, [Out] double[,] B);

        [DllImport(libraryName, EntryPoint = "call_slice_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_array_m(int rows, int columns, [In] double[,] A, int start_row, int end_row, int start_column, int end_column, [Out] double[,] B);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=x*A</code>.
        /// <code><![CDATA[subroutine call_mul_array_vm(n,m,x,A,b) bind(c)
        ///integer, intent(in), value :: n, m
        ///real(real64), intent(in) :: A(n,m), x(m)
        ///real(real64), intent(out) :: b(n)]]>
        /// </code>
        /// </summary>
        /// <param name="x">The known vector</param>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_vm(int rows, int columns, [In] double[] x, [In] double[,] A, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <c>C=A*B</c>
        /// <code><![CDATA[subroutine call_mul_array_mm(n,m,k,A,x,b) bind(c)
        ///integer, intent(in), value :: n, m, k
        ///real(real64), intent(in) :: A(n,m), x(m,k)
        ///real(real64), intent(out) :: b(n,k)]]>
        ///</code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix</param>
        /// <param name="C">The result matrix</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] x, [Out] double[,] b);

        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <c>A*x=b</c> for <paramref name="x"/>.
        /// <code><![CDATA[subroutine call_solve_array_mv(n,A,b,x) bind(c)
        ///use mod_array_inv
        ///integer, intent(in), value :: n
        ///real(real64), intent(in) :: A(n,n), b(n)
        ///real(real64), intent(out) :: x(n) ]]>
        /// </code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(libraryName, EntryPoint = "call_solve_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_solve_array_mv(int rows, [In] double[,] A, [In] double[] b, [Out] double[] x);
        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <c>A*X=B</c> for <paramref name="X"/>.
        /// <code><![CDATA[subroutine call_solve_array_mm(n,k,A,b,x) bind(c)
        ///integer, intent(in), value :: n,k
        ///real(real64), intent(in) :: A(n,n), b(n,k)
        ///real(real64), intent(out) :: x(n,k)]]>
        /// </code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix. </param>
        /// <param name="X">The unknown matrix.</param>
        [DllImport(libraryName, EntryPoint = "call_solve_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_solve_array_mm(int rows, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);
        [DllImport(libraryName, EntryPoint = "call_lu_array_block_solve_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_lu_array_block_solve_mm(int rows, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);

        [DllImport(libraryName, EntryPoint = "call_inverse_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_inverse_array_m(int size, [In] double[,] A, [Out] double[,] A_inv);
        #endregion

        #region Scalar Functions
        [DllImport(libraryName, EntryPoint = "radians_to_degrees", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern double radians_to_degrees(double angle_rad);

        [DllImport(libraryName, EntryPoint = "degrees_to_radians", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern double degrees_to_radians(double angle_deg);

        [DllImport(libraryName, EntryPoint = "acos_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern double acos_scalar(double c);
        #endregion

        #region Quaternion Functions
        [DllImport(libraryName, EntryPoint = "quat_normal_01", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_normal_01(ref int seed, [Out] double[] q);

        [DllImport(libraryName, EntryPoint = "quat_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_scalar([In] double[] q, ref double s);

        [DllImport(libraryName, EntryPoint = "quat_vector", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_vector([In] double[] q, [Out] double[] v);

        [DllImport(libraryName, EntryPoint = "quat_conjugate", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_conjugate(double[] q, [Out] double[] q2);

        [DllImport(libraryName, EntryPoint = "quat_exponentiate", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_exponentiate(double[] q1, [Out] double[] q2);

        [DllImport(libraryName, EntryPoint = "quat_inverse", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_inverse(double[] q, [Out] double[] q2);

        [DllImport(libraryName, EntryPoint = "quat_add", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_add(double[] q1, double[] q2, [Out] double[] q3);
        [DllImport(libraryName, EntryPoint = "quat_subtract", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_subtract(double[] q1, double[] q2, [Out] double[] q3);
        [DllImport(libraryName, EntryPoint = "quat_scale", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_scale(double x, double[] q2, [Out] double[] q3);
        [DllImport(libraryName, EntryPoint = "quat_multiply", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_multiply(double[] q1, double[] q2, [Out] double[] q3);

        [DllImport(libraryName, EntryPoint = "quat_multiply2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_multiply2(double[] q1, double[] q2, [Out] double[] q3);

        [DllImport(libraryName, EntryPoint = "quat_dot", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_dot(double[] q1, double[] q2, ref double q3);
        [DllImport(libraryName, EntryPoint = "quat_cross", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat_cross(double[] q1, double[] q2, [Out] double[] q3);
        [DllImport(libraryName, EntryPoint = "quat_norm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double quat_norm(double[] q);
        #endregion

        #region Rotation Functions
        [DllImport(libraryName, EntryPoint = "rotate_normal_01", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_normal_01(ref int seed, [Out] double[] q);

        [DllImport(libraryName, EntryPoint = "rotate_normalize", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_normalize([In] double[] qt, [Out] double[] q);

        [DllImport(libraryName, EntryPoint = "rotate_axis2mat", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_axis2mat([In] double[] axis, double angle, [Out] double[,] a);

        [DllImport(libraryName, EntryPoint = "rotate_axis2quat", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_axis2quat([In] double[] axis, double angle, [Out] double[] q);

        [DllImport(libraryName, EntryPoint = "rotate_axis_vector", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_axis_vector([In] double[] axis, double angle, double[] v, [Out] double[] w);

        [DllImport(libraryName, EntryPoint = "rotate_mat2axis", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_mat2axis(double[,] a, [Out] double[] axis, ref double angle);

        [DllImport(libraryName, EntryPoint = "rotate_mat2quat", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_mat2quat(double[,] a, [Out] double[] q);

        [DllImport(libraryName, EntryPoint = "rotate_quat_diag2mat", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat_diag2mat(double[] q, double[] d, [Out] double[,] a);

        [DllImport(libraryName, EntryPoint = "rotate_mat_vector", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_mat_vector(double[,] a, double[] v, [Out] double[] w);

        [DllImport(libraryName, EntryPoint = "rotate_quat2axis", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat2axis(double[] q, [Out] double[] axis, ref double angle);

        [DllImport(libraryName, EntryPoint = "rotate_quat2mat", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat2mat(double[] q, [Out] double[,] a);

        [DllImport(libraryName, EntryPoint = "rotate_quat2mat_inv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat2mat_inv(double[] q, [Out] double[,] a, bool inverse);

        [DllImport(libraryName, EntryPoint = "rotate_quat_vector", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat_vector(double[] q, double[] v, [Out] double[] w);

        [DllImport(libraryName, EntryPoint = "rotate_quat_vector_inv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void rotate_quat_vector_inv(double[] q, double[] v, [Out] double[] w, bool inverse);
        #endregion

        #region Cubic Splines

        [DllImport(libraryName, EntryPoint = "spline_calc_ypp_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine spline_calc_ypp_array(n,x,y,ypp) bind(c)
        internal static extern void spline_calc_ypp_array(int n, double[] x, double[] y, [Out] double[] ypp);

        [DllImport(libraryName, EntryPoint = "spline_calc_ypp_domain", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine spline_calc_ypp_domain(n,x_start, x_end,y,ypp) bind(c)
        internal static extern void spline_calc_ypp_domain(int n, double x_start, double x_end, double[] y, [Out] double[] ypp);

        [DllImport(libraryName, EntryPoint = "spline_intepolate_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine spline_intepolate_values(n,x,y,ypp,xe,ye,ype,yppe)
        internal static extern void spline_intepolate_values(int n, double[] x, double[] y, double[] ypp, double xe, [Out] out double ye, [Out] out double ype, [Out] out double yppe);

        [DllImport(libraryName, EntryPoint = "spline_interpolate_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine spline_interpolate_array(n,x,y,ypp,m,xe,ye,yppe) bind(c)
        internal static extern void spline_interpolate_array(int n, double[] x, double[] y, double[] ypp, int m, double[] xe, [Out] double[] ye, [Out] double[] yppe);

        [DllImport(libraryName, EntryPoint = "spline_interpolate_domain", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine spline_interpolate_domain(n,x,y,ypp,m,x_start,x_end,ye,yppe) bind(c)
        internal static extern void spline_interpolate_domain(int n, double[] x, double[] y, double[] ypp, int m, double x_start, double x_end, [Out] double[] ye, [Out] double[] yppe);
        #endregion

        #region Vector2
        [DllImport(libraryName, EntryPoint = "add_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 add_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "sub_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 sub_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_scalar_vec2(double s, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "mul_vec2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_vec2_scalar(in FVector2 a, double s);
        [DllImport(libraryName, EntryPoint = "inner_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double inner_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "outer_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 outer_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "cross_scalar_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 cross_scalar_vec2(double a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "cross_vec2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 cross_vec2_scalar(in FVector2 a, double b);
        [DllImport(libraryName, EntryPoint = "cross_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double cross_vec2_vec2(in FVector2 a, in FVector2 b);
        //      ======= VECTOR2 ========
        //      array_to_vec2
        //  *   add_vec2_vec2
        //  *   sub_vec2_vec2
        //      mul_array_vec2
        //  *   mul_scalar_vec2
        //      mul_vec2_array
        //  *   mul_vec2_scalar
        //      cross_vec2_scalar
        //      cross_vec2_vec2
        //  *   inner_vec2_vec2
        //      outer_vec2_vec2
        //      vec2_to_array
        //      vec2_zero
        //      outer_vec2_vec2
        #endregion

        #region Matrix2
        [DllImport(libraryName, EntryPoint = "add_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "sub_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "add_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "add_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "sub_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_mat2_vec2(in FMatrix2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "mul_vec2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_vec2_mat2(in FVector2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "inner_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 inner_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "trace_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double trace_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "determinant_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double determinant_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "transpose_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 transpose_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "inverse_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 inverse_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "solve_mat2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 solve_mat2_vec2(in FMatrix2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "solve_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 solve_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        //      ======= MAT2 ========
        //      array_to_mat2
        //  *   add_mat2_scalar
        //  *   add_scalar_mat2
        //  *   add_mat2_mat2
        //  *   sub_mat2_scalar
        //  *   sub_scalar_mat2
        //  *   sub_mat2_mat2
        //  *   mul_scalar_mat2
        //  *   mul_mat2_scalar
        //  *   mul_mat2_vec2
        //  *   mul_vec2_mat2
        //  *   mul_mat2_mat2
        //  *   solve_mat2_vec2
        //  *   solve_mat2_mat2
        #endregion

        #region Vector3
        [DllImport(libraryName, EntryPoint = "add_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 add_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "sub_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 sub_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_scalar_vec3(double s, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "mul_vec3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_vec3_scalar(in FVector3 a, double s);
        [DllImport(libraryName, EntryPoint = "inner_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double inner_vec3_vec3(in FVector3 a, in FVector3 b  );
        [DllImport(libraryName, EntryPoint = "outer_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 outer_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "cross_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double cross_vec3_vec3(in FVector3 a, in FVector3 b  );
        [DllImport(libraryName, EntryPoint = "trace_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double trace_mat3(in FMatrix3 fMatrix3               );
        [DllImport(libraryName, EntryPoint = "determinant_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double determinant_mat3(in FMatrix3 fMatrix3         );
        #endregion

        #region Matrix3
        [DllImport(libraryName, EntryPoint = "mul_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mul_scalar_mat3(double v, in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "add_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "sub_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "add_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_scalar_mat3(double a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_scalar_mat3(double a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "add_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "sub_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_mat3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_mat3_vec3(in FMatrix3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "mul_vec3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_vec3_mat3(in FVector3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "mul_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mul_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "inner_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 inner_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "transpose_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 transpose_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "inverse_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 inverse_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "solve_mat3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 solve_mat3_vec3(in FMatrix3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "solve_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 solve_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        //      ======= MAT3 ========
        //      array_to_mat3
        //      add_scalar_mat3
        //      add_mat3_scalar
        //      add_mat3_mat3
        //      sub_mat3_scalar
        //      sub_scalar_mat3
        //      sub_mat3_mat3
        //      mul_scalar_mat3
        //      mul_mat3_scalar
        //      mul_mat3_vec3
        //      mul_vec3_mat3
        //      mul_mat3_mat3
        //      solve_mat3_vec3
        //      solve_mat3_mat3
        #endregion

        #region Fortran Exports (uncategorized)
        #endregion

        #region Test Functions
        [DllImport(libraryName, EntryPoint = "call_quat_test_all", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_quat_test_all();

        #endregion

    }
}
