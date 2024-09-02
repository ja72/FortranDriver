namespace FortranDriver
{
    public static class HelperFunctions
    {
        public static double[,] BuildArray(int n, int m, Func<int, int, double> init = null)
        {
            var A = new double[n, m];
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < m; j++)
                {
                    if (init==null)
                    {
                        A[i, j] = 0.0;
                    }
                    else
                    {
                        A[i, j] = init(i, j);
                    }
                }
            }

            return A;
        }

        public static void ShowArray(this double[,] A, int width = 11, string formatting = "g4")
        {
            int rows = A.GetLength(0), cols = A.GetLength(1);

            for (int i = 0; i < rows; i++)
            {
                Console.Write("|");
                for (int j = 0; j < cols; j++)
                {
                    string text = A[i, j].ToString(formatting).PadLeft(width);
                    if (text.Length>width)
                    {
                        text = text.Substring(0, width-1) + "…";
                    }
                    Console.Write($" {text}");
                }
                Console.WriteLine(" |");
            }
            Console.WriteLine();
        }

    }
}
