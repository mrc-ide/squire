double squire_get_val(double x[], int n){
  for(int i = 0; i < sizeof(x); i++){
    for(int j = (i + 1); j < sizeof(x); j++){
      if(x[i] > x[j]){
        double xi = x[i];
        double xj = x[j];
        x[i] = xj;
        x[j] = xi;
      }
    }
  }
  return(x[n - 1]);
}
