// the release is built against cats 1.0.0-MF
package eu.timepit.refined

import _root_.cats.Show
import _root_.cats.instances.eq._
import _root_.cats.kernel.Eq
import _root_.cats.syntax.contravariant._
import eu.timepit.refined.api.RefType

package object cats {

  /**
    * `Eq` instance for refined types that delegates to the `Eq`
    * instance of the base type.
    */
  implicit def refTypeEq[F[_, _], T: Eq, P](implicit rt: RefType[F]): Eq[F[T, P]] =
    Eq[T].contramap(rt.unwrap)

  /**
    * `Show` instance for refined types that delegates to the `Show`
    * instance of the base type.
    */
  implicit def refTypeShow[F[_, _], T: Show, P](implicit rt: RefType[F]): Show[F[T, P]] =
    Show[T].contramap(rt.unwrap)
}
