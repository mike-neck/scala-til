/*
 * Copyright 2017 Shinya Mochida
 * 
 * Licensed under the Apache License,Version2.0(the"License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,software
 * Distributed under the License is distributed on an"AS IS"BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.example

trait Maybe extends Hk1 {
  def getValue: E
}

object NothingInt extends Maybe {
  override type E = Int
  override def getValue: Int = throw new NoSuchElementException("this is Nothing")
}

case class JustInt(value: Int) extends Maybe {
  override type E = Int
  override def getValue: Int = value
}

object NothingString extends Maybe {
  override type E = String
  override def getValue: String = throw new NoSuchElementException("this is Nothing")
}

case class JustString(value: String) extends Maybe {
  override type E = String
  override def getValue: String = value
}

// Functor instances

object MaybeIntToStringFunctor extends Functor {
  override type F = Maybe
  override type A = Int
  override type B = String

  override def map(o: Maybe { type E = Int }, f: Fun1 { type I = Int ;type O = String }): Maybe { type E = String } =
    o match {
      case NothingInt => NothingString
      case JustInt(v) => JustString(f(v))
    }
}

object MaybeStringToIntFunctor extends Functor {
  override type F = Maybe
  override type A = String
  override type B = Int

  override def map(o: Maybe { type E = String }, f: Fun1 { type I = String ;type O = Int }): Maybe { type E = Int } =
    o match {
      case NothingString => NothingInt
      case JustString(v) => JustInt(f(v))
    }
}
