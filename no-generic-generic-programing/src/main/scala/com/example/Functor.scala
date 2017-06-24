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

trait Functor {self =>
  type F <: Hk1
  type A
  type B

  //noinspection ApparentRefinementOfResultType
  def map(o: F {type E = self.A}, f: Fun1 {type I = self.A; type O = self.B}): F { type E = self.B }

  def map(f: Fun1 {type I = self.A; type O = self.B}): Fun1 {
    type I = F { type E = self.A }
    type O = F { type E = self.B }
  } = new Fun1 {
    override type I = F { type E = self.A }
    override type O = F { type E = self.B }

    override def apply(input: F { type E = self.A }): F { type E = self.B } = self.map(input, f) 
  }  
}
