using System.Runtime.InteropServices;

namespace FortranDriver
{
    internal static class FortranMethods
    {
        // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
        //       delcaration instead. Use a ref to first element instead of passing a 2D array.

        const string libraryName = "FortranDriverDLL";

        // array_round_m(n,m,x,d,r)
        [DllImport(libraryName, EntryPoint = "array_round_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_round_v(int size, [In] double[] x, int digits, [Out] double[] r);
        [DllImport(libraryName, EntryPoint = "array_round_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_round_m(int rows, int columns, [In] double[,] x, int digits, [Out] double[,] r);

        [DllImport(libraryName, EntryPoint = "array_norm_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_norm_v(int size, [In] double[] A, ref double s);

        [DllImport(libraryName, EntryPoint = "array_slice_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_slice_v(int size, [In] double[] A, int start_index, int end_index, [Out] double[] B);

        [DllImport(libraryName, EntryPoint = "array_random_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_random_v(int size, [In] double minValue, [In] double maxValue, [Out] double[] A);
        [DllImport(libraryName, EntryPoint = "array_uniform_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_uniform_v(int size, ref int seed, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "array_elem_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_elem_v(int size, int index, [In] double x, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "array_add_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_add_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "array_subtract_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_subtract_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "array_scale_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_scale_v(int size, double x, [In] double[] y, [Out] double[] z);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="x">The first vector.</param>
        /// <param name="y">The second vector.</param>
        /// <param name="z">The dot product result.</param>
        [DllImport(libraryName, EntryPoint = "array_dot_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_dot_v(int size, [In] double[] x, [In] double[] y, [Out] out double z);


        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <code>A*x=b</code> for <paramref name="x"/>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(libraryName, EntryPoint = "array_solve_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_solve_mv(int rows, [In] double[,] A, [In] double[] b, [Out] double[] x);
        [DllImport(libraryName, EntryPoint = "array_block_solve_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_block_solve_mv(int rows, [In] double[,] A, [In] double[] b, [Out] double[] x);

        [DllImport(libraryName, EntryPoint = "array_random_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_random_m(int rows, int columns, [In] double minValue, [In] double maxValue, [Out] double[,] A);
        [DllImport(libraryName, EntryPoint = "array_uniform_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_uniform_m(int rows, int columns, ref int seed, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_diag_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_diag_m(int size, [In] double[] x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_scalar_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_scalar_m(int rows, int columns, [In] double x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_norm_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_norm_m(int rows, int columns, [In] double[,] A, ref double s);

        [DllImport(libraryName, EntryPoint = "array_add_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_add_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_subtract_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_subtract_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_scale_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_scale_m(int rows, int columns, double x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_det_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_det_m(int size, [In] double[,] A, [Out] out double det);

        [DllImport(libraryName, EntryPoint = "array_tansp_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_tansp_m(int rows, int columns, [In] double[,] A, [Out] double[,] At);

        [DllImport(libraryName, EntryPoint = "array_reshape_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_reshape_mv(int rows, int columns, [In] double[,] A, int new_size, [Out] double[] B);
        [DllImport(libraryName, EntryPoint = "array_reshape_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_reshape_vm(int size, [In] double[] A, int new_rows, int new_columns, [Out] double[,] B);
        [DllImport(libraryName, EntryPoint = "array_reshape_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_reshape_mm(int rows, int columns, [In] double[,] A, int new_rows, int new_columns, [Out] double[,] B);

        [DllImport(libraryName, EntryPoint = "array_slice_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_slice_m(int rows, int columns, [In] double[,] A, int start_row, int end_row, int start_column, int end_column, [Out] double[,] B);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "array_product_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_product_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=x*A</code>.
        /// </summary>
        /// <param name="x">The known vector</param>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "array_product_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_product_vm(int rows, int columns, [In] double[] x, [In] double[,] A, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>C=A*B</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix</param>
        /// <param name="C">The result matrix</param>
        [DllImport(libraryName, EntryPoint = "array_product_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_product_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] C);

        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <code>A*X=B</code> for <paramref name="X"/>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix. </param>
        /// <param name="X">The unknown matrix.</param>
        [DllImport(libraryName, EntryPoint = "array_solve_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_solve_mm(int rows, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);
        [DllImport(libraryName, EntryPoint = "array_block_solve_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_block_solve_mm(int rows, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);

        [DllImport(libraryName, EntryPoint = "array_inverse_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_inverse_m(int size, [In] double[,] A, [Out] double[,] A_inv);


    }
}
