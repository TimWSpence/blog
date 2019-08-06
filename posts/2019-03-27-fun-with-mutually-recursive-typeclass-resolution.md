---
title: Fun with mutually-recursive typeclass resolution
---

At Permutive we’ve recently started experimenting with GraphQL, via Sangria.
During that process I’ve had to write a few instances of Sangria'sInputType by
hand, which had me wondering if they could be derived automatically. I
subsequently discovered Sangria supports doing this out of the box (in a
slightly more verbose way) but it’s a fun way of demonstrating the power of
Shapeless (and the Scala compiler) so I thought I’d write about it anyway!

## The problem

Sangria requires you to define values which describe the input types you accept.
For example, to accept a list of key-value pairs you might write:

```scala
case class KeyValue(key: String, value: String)

val inputType = ListInputType(InputObjectType[KeyValue](
    name = "KeyValue",
    fields = List(
      InputField("key", StringType),
      InputField("value", StringType)
    )
  ))
```

This manual control is sometimes useful as it allows you to add metadata like
descriptions of the fields. However, we found that in most instances our input
types were sufficiently simple that we would prefer to scrap the boilerplate and
use an API that’s more like:

```scala
val inputType = deriveInputType[List[KeyValue]]
```

The fact that the Sangria code is basically a manual transcription of the type
we want it to represent would suggest that this should be possible. Can we do
it?!

## A first attempt

If our domain type representing our input is A then the value we want to derive
has a type that looks very like InputType[A] so our first attempt might look
something like this:

```scala
sealed trait ToInputType[A] {

  def to: InputType[A]

}
```

This was, in fact, the first encoding I attempted but it has a couple of
problems. If we examine the Sangria types more closely we find:

```scala
val StringType: InputType[String @@ CoercedScalaResult] //via some indirection

case class ListInputType[T](ofType: InputType[T]) extends InputType[Seq[T]]
```

Our encoding has failed to capture the @@ CoercedScalaResult annotation on
Strings (which it turns out breaks things!) and would force us to use raw Seq
instead of List for our domain classes. It would appear that our derived
InputType is actually parameterised not on A but on a type that is a function of
A (a dependent type)!

## Dependent types to the rescue!

Instead we try the following encoding (with some basic instances but with the
definition of LowestPriorityInstances intentionally omitted for the moment):

```scala
sealed trait ToInputType[A] {
  type Repr
  def to: InputType[Repr]
}

object ToInputType extends LowPriorityInstances with LowestPriorityInstances {

  type Aux[A, R] = ToInputType[A] {type Repr = R}
  
  implicit val stringToInputType: ToInputType.Aux[String, String @@ CoercedScalaResult] =
    new ToInputType[String] {
      override type Repr = String @@ CoercedScalaResult
      override def to: InputType[Repr] = StringType
    }
  
  implicit def optionToInputType[A, R](implicit toInput: ToInputType.Aux[A, R]):
    ToInputType.Aux[Option[A], Option[R]] =
    new ToInputType[Option[A]] {
      override type Repr = Option[R]
      override def to: InputType[Repr] = OptionInputType(toInput.to)
    }


  implicit def listToInputType[A, R](implicit toInput: ToInputType.Aux[A, R]):
    ToInputType.Aux[List[A], Seq[R]] =
    new ToInputType[List[A]] {
      override type Repr = Seq[R]
      override def to: InputType[Repr] = ListInputType(toInput.to)
    }
}

trait LowPriorityInstances {

  implicit def seqToInputType[A, R](implicit toInput: ToInputType.Aux[A, R]):
    ToInputType.Aux[Seq[A], Seq[R]] =
    new ToInputType[Seq[A]] {
      override type Repr = Seq[toInput.Repr]
      override def to: InputType[Repr] = ListInputType(toInput.to)
    }
}
```

As you can see, we can provide instances for basic types directly and can
recursively resolve them for `Option` , `List` , etc

But what do we do about composite domain classes? For that (see our motivating
example), we need to derive a List[InputField[_]]. Let’s try defining a
typeclass to produce such a value and then see what we can do with it:

```scala
trait ToFieldList[A] {

  def to: List[InputField[_]]

}
```

We now need to get a list of fields from our domain class and turn them into a
list of `InputField[_]`. We can use Shapeless for that!

```scala
trait LowestPriorityInstances {

  implicit def toInputTypeViaGeneric[A, Repr](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    toFieldList: Lazy[ToFieldList[Repr]],
    tag: ClassTag[A]): ToInputType.Aux[A, A @@ InputObjectResult] =
  new ToInputType[A] {
    override type Repr = A @@ InputObjectResult

    override def to: InputType[Repr] =
      InputObjectType[A](
        name = tag.toString.split("\\.").last,
        fields = toFieldList.value.to
      )

  }

}
```

This slightly scary piece of code uses Shapeless’s Generic to obtain a
representation of our domain class as an HList (heterogeneous list) of the field
types. The Labelled bit means that we can extract the names of the fields and
the Lazy helps to stop the Scala compiler giving up prematurely when recursively
resolving implicits. If you want to know more about Shapeless, I highly
recommend [this book](https://underscore.io/books/shapeless-guide/) from our
friends at Underscore!

Note that here we have mutually recursive typeclass resolution between
`ToInputType` and `ToFieldList` 🙀

All that remains is to derive an instance of `ToFieldList` for an arbitrary HList
and we should be done with our typeclass resolution! 

```scala
object ToFieldList {

  implicit val hnilToFieldList: ToFieldList[HNil] = new ToFieldList[HNil] {
    override def to: List[InputField[_]] = Nil
  }

  implicit def hlistToFieldList[K <: Symbol, R, H, T <: HList](
    implicit witness: Witness.Aux[K],
    headToInput: Lazy[ToInputType.Aux[H, R]],
    tailToFieldList: ToFieldList[T]
  ): ToFieldList[FieldType[K, H] :: T] = new ToFieldList[FieldType[K, H] :: T] {
    override def to: List[InputField[_]] =
      InputField(witness.value.name, headToInput.value.to) :: tailToFieldList.to
  }

}
```

Lastly, we need a function to summon the appropriate instance:

```scala
package object derivation {

  def deriveInputType[A](implicit toInput: ToInputType[A]): InputType[toInput.Repr] =
    toInput.to

}
```

And there you go, we can call the following, as promised!

```scala
val inputType = deriveInputType[List[KeyValue]]
```

[Here](https://gist.github.com/TimWSpence/57f02cb06aa6a193e9485794aa06ee92) is a
gist if you want to see all of the code at once.
