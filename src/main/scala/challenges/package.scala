package com.eddsteel.advent17

package object challenges {
  type ErrorOr[A] = Either[String, A]
  // "a parser of things is a function from string to an
  // option of tuple of thing and String!"
  type Parser[A] = String => Option[(A, String)]
}
