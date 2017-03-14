package apdl.core

import scala.language.higherKinds
import scala.virtualization.lms.common._

/**
  * Created by snipy
  * Project apdl
  */
trait APDLBase extends Base {
  type Source
  type Sampling
  type SamplingType
  type SamplingRate
  type TimeValue
  type TimeUnit
  type Transformation[A, B]
  type Communication
  type DataType[A]

  def new_Source(name: Rep[String]): Rep[Source]

  def new_Sampling(name: Rep[String], samplingType: Rep[SamplingType], samplingRate: Rep[SamplingRate]): Rep[Sampling]

  def new_SamplingType(): Rep[SamplingType]

  def new_SamplingRate(timeValue: Rep[TimeValue], timeUnit: Rep[TimeUnit]): Rep[SamplingRate]

  def new_Transformation[A, B](f: Rep[A => B]): Rep[Transformation[A, B]]

  def new_Communication(name: Rep[String]): Rep[Communication]

  def new_DataType[A](name: Rep[String]): Rep[DataType[A]]
}

trait APDLProg extends APDLBase{
  def progMain(): Rep[Unit]
}

trait APDL extends App {

}