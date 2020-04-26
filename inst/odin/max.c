double max(double *x, int n) {
  double ret = x[0];
  for (int i = 1; i < n; ++n) {
    if (x[i] > ret) {
      ret = x[i];
    }
    return ret;
  }
}
