package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case PushNumI(f) => { (Num(f) :: stack, env) }
            case PushBoolI(b) => { (Bool(b) :: stack, env) }
            case PopI => { 
                if (stack.isEmpty) {
                    throw new RuntimeException(s"Stack Empty")
                } else {
                    (stack.tail, env)
                }
            }
            case AddI => {
                val v1 = stack.head
                val v2 = stack.tail.head
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        (Num(b + a) :: stack.tail.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Add failed")
                }

            }
            case SubI => {
                val v1 = stack.head
                val v2 = stack.tail.head
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        (Num(b - a) :: stack.tail.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Sub failed")
                }
            }
            case MultI => {
                val v1 = stack.head
                val v2 = stack.tail.head
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        (Num(b * a) :: stack.tail.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Sub failed")
                }
            }
            case DivI => {
                val v1 = stack.head
                val v2 = stack.tail.head
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        if (b != 0) {
                            (Num(b / a) :: stack.tail.tail, env)
                        } else {
                            throw new RuntimeException(s"Sub failed: Cannot Divide by 0")
                        }
                    }
                    case _ => throw new RuntimeException(s"Sub failed")
                }
            }
            case LogI => {
                stack match {
                    case Num(a) :: rest => {
                        if ( a > -1 ) {
                            PushNumI(Math.log(a))
                            (Num(Math.log(a)) :: stack.tail, env)
                        } else {
                            throw new RuntimeException(s"Log val not positive")
                        }
                    }
                    case _ => throw new RuntimeException(s"Log failed")
                }
            }
            case ExpI => {
                stack match {
                    case Num(a) :: rest => {
                        (Num(Math.pow(Math.E,a)) :: stack.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Exp failed")
                }
            }
            case SinI => {
                stack match {
                    case Num(a) :: rest => {
                        (Num(Math.sin(a)) :: stack.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Exp failed")
                }
            }
            case CosI => {
                stack match {
                    case Num(a) :: rest => {
                        (Num(Math.cos(a)) :: stack.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Exp failed")
                }
            }
            case GeqI => {
                val v1 = stack.head
                val v2 = stack.tail.head
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        (Bool(b >= a) :: stack.tail.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Sub failed")
                }
            }
            case EqI => {
                val v1 = env.head._2
                val v2 = env.head._2
                (v1, v2) match {
                    case (Num(a), Num(b)) => {
                        (Bool(a == b) :: stack.tail.tail, env)
                    }
                    case (Bool(a), Bool(b)) => {
                        (Bool(a == b) :: stack.tail.tail, env)
                    }
                    case _ => {
                        (Bool(false) :: stack, env)
                    }
                }
            }
            case NotI => {
                stack match {
                    case Bool(a) :: rest => {
                        (Bool(!a) :: stack.tail, env)
                    }
                    case _ => throw new RuntimeException(s"Not failed")
                }
            }
            case LoadEnv(s) => {
                stack match {
                    case _ :: rest => (stack.tail, (s, stack.head) :: env)
                    case _ => throw new RuntimeException(s"LoadEnv failed")
                }
            }
            case StoreEnv(s) => {
                env.find(_._1 == s) match {
                    case Some( (_, a) ) => (a :: stack, env)
                    case None => throw new RuntimeException(s"StoreEnv failed")
                }
            }
            case PopEnv => {
                env match {
                    case _ :: rest => (stack, env.tail)
                    case _ => throw new RuntimeException(s"PopEnv failed")
                }
            }
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}