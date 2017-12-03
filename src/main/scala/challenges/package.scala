package com.eddsteel.advent17

package object challenges {
  type ErrorOr[A] = Either[String, A]
}
