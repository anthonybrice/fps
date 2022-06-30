package fps.chapter3

object Exercise7 {
  // No, foldRight cannot short circuit. One could write the given op such that
  // it is a noop after 0.0 is encountered, but foldRight must still traverse
  // the entire list applying the op.

}
