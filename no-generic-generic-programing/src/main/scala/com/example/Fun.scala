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

trait Fun {
  type O

  def apply: O
}

trait Fun1 {
  type I
  type O

  def apply(i: I): O
}

trait Fun2 {self =>
  type I1
  type I2

  type O

  def apply(i1: I1, i2: I2): O
  def apply(i1: I1): Fun1 { type I = self.I2; type O = self.O } = new Fun1 {

    override type I = self.I2
    override type O = self.O

    override def apply(i2: self.I2): O = self.apply(i1, i2)
  }
}
