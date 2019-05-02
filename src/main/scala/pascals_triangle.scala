   /* Get Coordinate of Pascals triangle
      Index starts from 0
      run like import pascals_triangle._
      pascal(0,2)
      pascal(1,2)
      pascal(1,3)
      pascal(2,3)

   */
object pascals_triangle {
     def pascal(c: Int, r: Int): Int = {
       if(c==0 || c==r)
         1
       else
         pascal(c-1,r-1) + pascal(c,r-1)
     }

}
