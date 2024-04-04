import { useEffect, useState } from 'react'
import React, { JSX } from 'react'
import Demonaco from './Demonaco'

const pantograph_url = "./pantograph.html"
const mode = 'mixed' as 'mixed' | 'pantograph' | 'text';

export type BiExercise = { instructions: JSX.Element, text_program: string, pantograph_program_index: string }
export type Exercise
  = { case: 'pantograph', instructions: JSX.Element, program_index: string }
  | { case: 'text', instructions: JSX.Element, program: string }

export default function App() {
  useEffect(() => {
    console.log("[pantograph-user-study]")
  })

  const [exercises, set_exercises] = useState<Exercise[] | undefined>(undefined);

  function initExercises(start_with_pantograph: boolean) {
    switch (mode) {
      case 'mixed': {
        if (start_with_pantograph) {
          set_exercises(all_biexercises.map((ex, i) => i < all_biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions, program_index: ex.pantograph_program_index } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
        } else {
          set_exercises(all_biexercises.map((ex, i) => i >= all_biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions, program_index: ex.pantograph_program_index } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
        }
        break;
      }
      case 'text': {
        set_exercises(all_biexercises.map((ex) => ({ case: 'text', instructions: ex.instructions, program: ex.text_program })));
        break;
      }
      case 'pantograph': {
        set_exercises(all_biexercises.map((ex) => ({ case: 'pantograph', instructions: ex.instructions, program_index: ex.pantograph_program_index })));
        break;
      }
    }
  }

  function initGroupA() {
    initExercises(true)
  }

  function initGroupB() {
    initExercises(false)
  }

  // const [exercise_status, set_exercise_status] = useState<'text-tutorial' | 'begin' | number | 'end'>('text-tutorial');
  const [exercise_status, set_exercise_status] = useState<'text-tutorial' | 'begin' | number | 'end'>('text-tutorial');

  function renderCurrentInstruction(): JSX.Element {
    if (typeof exercise_status === 'number') {
      if (exercises === undefined) return renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `exercises === undefined`"}</div>)
      else if (!(0 <= exercise_status && exercise_status < exercises.length)) renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`"}</div>)
      else {
        const exercise = exercises[exercise_status];
        return (
          <div
            style={{
              padding: "0.5em",
              userSelect: "none",
            }}
          >
            {exercise.instructions}
          </div>
        );
      }
    } else {
      switch (exercise_status) {
        case 'text-tutorial': return renderTextTutorial();
        case 'begin': return renderBegin();
        case 'end': return renderEnd();
      }
    }

    return (<></>)
  }

  function renderCurrentExercise(): JSX.Element {
    if (typeof exercise_status === 'number') {
      if (exercises === undefined) return renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `exercises === undefined`"}</div>)
      else if (!(0 <= exercise_status && exercise_status < exercises.length)) renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`"}</div>)
      else {
        const exercise = exercises[exercise_status];
        switch (exercise.case) {
          case 'text': {
            return (<Demonaco key={exercise_status} start_program={exercise.program} />)
          }
          case 'pantograph': {
            return (<iframe
              key={exercise_status}
              style={{
                width: "100%",
                height: "100%",
                border: "none",
                margin: "0",
                padding: "0",
              }}
              src={`${pantograph_url}?UserStudyProgramIndex=${exercise.program_index}`}
            />);
          }
        }
      }
    } else if (exercise_status === 'text-tutorial') {
      return (<Demonaco key={exercise_status} start_program={`let a : Int = 1 in
let a2 = a * a in
let square = fun x => x * x in

let l = cons 0 (cons 1 (cons 2 (cons 3 nil))) in
let isNil : List Int -> Bool = 
    fun l =>
        match l with
        | nil => true
        | cons h t => false
in

square ? == a2
`} />)
    }

    return (<></>)
  }

  function renderControls(): JSX.Element {
    const nextExercise = () => {
      set_exercise_status(i => {
        const next_exercise_status = (() => {
          switch (i) {
            case 'text-tutorial': return 'begin';
            case 'begin': return 0;
            case 'end': return 'end';
            default: {
              if (exercises === undefined) throw new Error("invariant violated: exercise_ix: number ==> exercises !== undefined");
              return i == exercises.length - 1 ? 'end' : i + 1
            }
          }
        })();
        console.log("next_exercise_ix", next_exercise_status);
        if (exercises !== undefined && typeof next_exercise_status === 'number') {
          console.log("exercises[next_exercise_ix].case", exercises[next_exercise_status].case)
        }
        return next_exercise_status
      })
    }

    const renderContainer = (kids: JSX.Element[]) => (
      <div
        style={{
          padding: "0.5em",
          display: "flex",
          flexDirection: "row",
          gap: "0.5em"
        }}
      >
        {kids}
      </div>
    )

    switch (exercise_status) {
      case 'text-tutorial': return renderContainer([
        <button onClick={nextExercise}>
          next
        </button>
      ])
      case 'begin': return renderContainer([
        <button onClick={() => {
          initGroupA()
          nextExercise()
        }}>
          Begin Group A
        </button>,
        <button onClick={() => {
          initGroupB()
          nextExercise()
        }}>
          Begin Group B
        </button>
      ])
      case 'end': return renderContainer([])
      default: return renderContainer([
        <button onClick={nextExercise}>
          next
        </button>
      ])
    }
  }

  const renderTextTutorial = () => (
    <div
      style={{
        padding: "0.5em",
        display: 'flex',
        flexDirection: 'column',
        gap: "0.5em",
      }}
    >
      {renderExerciseTitle("Text Editor Introduction")}
      <div>
        To the left of these instructions is a text editor with the basic editing features of programming-focussed text editors such as VS Code, Sublime Text, etc.
        This text editor also has a few extra features:
        <ul>
          <li>Checks syntax automatically</li>
          <li>Checks types automatically</li>
          <li>Hover over errors for more info</li>
        </ul>
      </div>
      <div>
        The programming language used with the text editor is essentially the same as the one you used in the Pantograph tutorial, with a few variations:
        <ul>
          <li>A hole is written {renderCode("?")}</li>
          <li>Hover over a hole to see its expected type</li>
          <li>Type annotations are <i>optional</i></li>
        </ul>
      </div>
      <div>
        Take a few minutes to get familiar with the text editor. For example:
        <ul>
          <li>Write a simple expression e.g. {renderCode("let x = 4 in ? x x")}</li>
          <li>Note syntax error feedback on hover</li>
          <li>Note type error feedback on hover</li>
          <li>Write a hole and view its type on hover</li>
        </ul>
      </div>
    </div>
  )

  const renderBegin = () => (
    <div
      style={{
        padding: "0.5em",
        display: 'flex',
        flexDirection: 'column',
        gap: "0.5em",
      }}
    >
      <div>
        <div>
          Welcome to the exercise section of the Pantograph user study.
          In this section, you will be presented with a selection of programming exercises.
          For half of the exercises you will use a text editor and for the other half you will use Pantograph.
          You will screen-record your session using <a href="https://recordscreen.io/" target='_blank'>screenrecord.io</a>. <b>Make sure to test that screen recording works before you begin.</b>
          <br></br>
          <br></br>
          You may ask the users study hosts any questions about the programming language, editors, or exercise instructions.
          However, they can't help you to solve an exercise.
          <br></br>
          <br></br>
          Once the study's hosts have announced that you may begin, start screen recording and then press the "Begin" button above according to your assigned group.
        </div>
        {/* <ul>
              <li>We can explain how to do any particular editing action in Pantograph or the text editor.</li>
              <li>We can explain anything about the programming language.</li>
              <li>We <b>cannot</b> give you any hints towards how to answer a question.</li>
              <li>We can explain more about what a question is asking.</li>
            </ul>
            <div>
              Before clicking the start button, ensure the following:
            </div>
            <ul>
              <li>You have completed the programming language tutorial.</li>
              <li>You have completed the Pantograph tutorial.</li>
              <li>You have experience using some kind of text editor for programming (e.g. VSCode, Sublime Text, etc.).</li>
              <li>This study&apos;s hosts have announced that you may begin this section (the exercise section) of this study.</li>
              <li>You have begun screen recording.</li>
            </ul>
        */}
      </div>
    </div>
  )


  const renderEnd = () => (
    <div
      style={{
        backgroundColor: "lightgreen",
        padding: "1em",
      }}
    >
      <div>
        <b>{"You're done with the exercise section of this study!"}</b>
      </div>
      <div>
        Please end your screen recording.
      </div>
    </div>
  )

  return (
    <div
      style={{
        height: "100vh",
        width: "100vw",
        display: "flex",
        flexDirection: "row-reverse",
      }}>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          flexGrow: 0,
          flexShrink: 0,
          width: "30em",
          overflow: "scroll",
        }}
      >
        <div
          style={{
            padding: "0.5em",
            height: "1.5em",
            backgroundColor: "black",
            color: "white",
          }}
        >Pantograph User Study</div>
        {renderControls()}
        {renderCurrentInstruction()}
      </div>
      <div
        style={{
          flexGrow: 1,
          flexShrink: 0,
          boxShadow: "-1px 0 0 0 black inset"
        }}
      >
        {renderCurrentExercise()}
      </div>
    </div>
  )
}

function renderInstruction(body: JSX.Element): JSX.Element {
  return (
    <div
      style={{
        padding: "0.5em",
        maxWidth: "40em",
      }}
    >
      {body}
    </div>
  )
}

const renderCodeblock = (text: string) => (
  <div
    style={{
      padding: "0.5em",
      margin: "0.5em 0",
      backgroundColor: "rgba(0, 0, 0, 0.1)",
      boxShadow: "1px 1px 4px 0 black"
    }}
  >
    <code style={{ whiteSpace: "pre" }}>{text}</code>
  </div>
)

const renderCode = (text: string) => (
  <code
    style={{
      padding: "0 0.2em",
      backgroundColor: "rgba(0, 0, 0, 0.1)",
    }}
  >{text}
  </code>
)

const renderExerciseTitle = (text: string) => (
  <div
    style={{
      fontSize: "1.2em",
      textDecoration: "underline",
      marginBottom: "0.5em"
    }}
  >
    {text}
  </div>
)

export const all_biexercises: BiExercise[] = [
  {
    instructions: (
      <div>
        {renderExerciseTitle("Transcribe and Edit")}
        <div><i>(<b>Do not</b> copy text from these instructions)</i></div>
        <div>Transcribe the following program into your editor. Whitespace does not have to be exact.
          {renderCodeblock(
            [
              "let f : Int -> Int = fun x : Int => 4 * x in",
              "let m : Int = 1 + 1 in",
              "let y : Int = f m in",
              "y / 2"
            ].join("\n")
          )}
          Then, edit the program to result in this (Move the definitions of {renderCode("f")} and {renderCode("m")} inside the definition of {renderCode("y")}):
          {renderCodeblock(
            [
              "let y : Int =",
              "    let f : Int -> Int = fun x : Int => 4 * x in",
              "    let m : Int = 1 + 1 in",
              "    f m in",
              "y / 2"
            ].join("\n")
          )}
        </div>
      </div>
    ),
    text_program: "",
    pantograph_program_index: "transcribe1",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Transcribe and Edit")}
        <div><i>(<b>Do not</b> copy text from these instructions)</i></div>
        <div>Transcribe the following program into your editor. Whitespace does not have to be exact.
          {renderCodeblock(
            [
              "fun x : Int => ",
              "    let i : Int = 7 in",
              "    x / i",
            ]
              .join("\n")
          )}
          Then, edit the program to result in this (move the function into the definition of a let expression):
          {renderCodeblock(
            [
              "let f : Int -> Int =",
              "    fun x : Int => ",
              "        let i : Int = 7 in",
              "        x / i in",
              "f 7",
            ]
              .join("\n")
          )}</div>
      </div>),
    text_program: "",
    pantograph_program_index: "transcribe2",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("DeMorgan's Law")}
        <div>
          You have been provided with an <i>incorrect</i> implementation of {renderCode("deMorgansLaw")}, which should be a function that takes as input two {renderCode("Bool")}s and output whether or not DeMorgan&apos;s Law holds for the inputs. Recall that DeMorgan&apos;s Law states that
          {renderCodeblock("!(p && q) == !p || !q")}
        </div>
        <br />
        <div>
          Edit {renderCode("deMorgansLaw")} to be correct.
        </div>
        <br />
        <div>
          Run should output {renderCode("true")}.
        </div>
      </div>
    ),
    text_program: `let deMorgansLaw : Bool -> Bool -> Bool =
    fun p => fun q =>
      p && q == p || q
in

deMorgansLaw true true &&
deMorgansLaw true false &&
deMorgansLaw false true &&
deMorgansLaw false false`,
    pantograph_program_index: "deMorgan",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Collatz")}
        <div>
          You have been provided wth a <i>buggy</i> implementation of {renderCode("collatz")}, which should be a function that takes as input an integer {renderCode("n : Int")} and outputs the number of steps there are in the Collatz sequence starting from {renderCode("n")}.
          Recall that the Collatz sequence is defined as follows: given an element of the Collatz sequence {renderCode("n")}, the next element of the Collatz sequence is:
          <ul>
            <li>if {renderCode("n")} is {renderCode("1")}, then this is the end of the Collatz sequence</li>
            <li>if {renderCode("n")} is even, then {renderCode("n / 2")}</li>
            <li>if {renderCode("n")} is odd, then {renderCode("n * 3 + 1")}</li>
          </ul>
        </div>
        <br />
        <div>
          Edit {renderCode("collatz")} to be correct.
        </div>
        <br />
        <div>
          Run should output {renderCode("5")}.
        </div>
      </div>
    ),
    text_program: `let collatz : Int -> Int =
    fun n =>
      if n == 1
      then 0
      else if (n % 2) == 0
      then collatz (n / 2)
      else (collatz (n + 1)) * 3
in

collatz 5`,
    pantograph_program_index: "collatz",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Prime")}
        <div>
          You have been provided with a <i>partial</i> implementation of {renderCode("isPrime")}, which should be a function that takes as input an integer {renderCode("n : Int")} and outputs whether or not {renderCode("n")} is a prime.
        </div>
        <br />
        <div>
          Finish implementing {renderCode("isPrime")} and fix any bugs.
        </div>
        <br />
        <div>
          Run should output {renderCode("true")}.
        </div>
      </div>
    ),
    text_program: `let isPrime : Int -> Bool =
    let helper : Int -> Bool =
        fun x =>
            if x == 1 then false
            else if (? % x) == ? then true
            else helper (x - 1) in
    fun n => ! (helper (n - 1))
in

! (isPrime 1) &&
isPrime 2 &&
isPrime 3 &&
! (isPrime 4) &&
isPrime 5 &&
! (isPrime 6) &&
isPrime 7 &&
! (isPrime 8)
`,
    pantograph_program_index: "isPrime",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Reverse")}
        <div>You have been provided with a stub for {renderCode("reverse")}, which should be a function that takes as input a list {renderCode("ls : List Int")} and outputs the reversed {renderCode("List Int")} -- that is, the {renderCode("List Int")} which has all the same elements as {renderCode("ls")} but in reversed order.</div>
        <br />
        <div>
          Run should output {renderCode("(cons 4 (cons 3 (cons 2 (cons 1))))")}.
        </div>
        <br />
        <div>
          <i>Note.</i> There is a built-in function {renderCode("append : List Int -> List Int -> List Int")} which appends two {renderCode("List Int")}s together.
        </div>
        <br />
        <div>
          <i>Note.</i> You can match on a list {renderCode("ls")} with the following syntax:
          {renderCodeblock(`match ls with
    | nil => ...
    | cons h t => ...`)}
        </div>
      </div>

    ),
    text_program: `let reverse : List Int -> List Int =
    ?
in

reverse (cons 1 (cons 2 (cons 3 (cons 4 nil))))
`,
    pantograph_program_index: "reverse",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Filter")}
        <div>You have been provided with a stub for {renderCode("filter")}, which should be a function that takes as input a condition {renderCode("cond : Int -> Bool")} and a list {renderCode("ls : List Int")}, and output a {renderCode("List Int")} which is the same as {renderCode("ls")} except without each element {renderCode("x")} such that {renderCode("not (cond x)")}.</div>
        <br />
        <div>
          Run should output {renderCode("(cons 2 (cons 4 nil))")}.
        </div>
      </div>
    ),
    text_program: `let filter : (Int -> Bool) -> List Int -> List Int =
    ?
in

filter (fun x => (x % 2) == 0) (cons 1 (cons 2 (cons 3 (cons 4 nil))))
`,
    pantograph_program_index: "filter",
  },
  {
    instructions: (
      <div>
        {renderExerciseTitle("Sum via Fold")}
        <div>
          You have been provided with an implementation of {renderCode("fold")}, which is a function that folds over a {renderCode("List Int")} to produce an {renderCode("Int")} result.
          You have also been provided with a stub for a function {renderCode("sum")}, which should compute the sum of a {renderCode("List Int")}.
          Implement {renderCode("sum")} by using {renderCode("fold")}.
          Note that, by using {renderCode("fold")} correctly, you will <i>not</i> need to {renderCode("match")} on the input {renderCode("List")}.
        </div>
        <br />
        <div>
          Run should output {renderCode("10")}.
        </div>
      </div>
    ),
    text_program: `let fold : (Int -> Int -> Int) -> Int -> List Int -> Int =
    fun f => fun n => fun ls =>
        match ls with
        | nil => n
        | cons h t => fold f (f n h) t
in

let sum : List Int -> Int =
    ?
in

sum (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
`,
    pantograph_program_index: "fold",
  },
]