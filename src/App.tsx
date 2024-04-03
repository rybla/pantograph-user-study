import { useEffect, useState } from 'react'
import React, { JSX } from 'react'
import Demonaco from './Demonaco'

const pantograph_url = "./pantograph.html"

export type BiExercise = { instructions: JSX.Element, text_program: string }
export type Exercise
  = { case: 'pantograph', instructions: JSX.Element }
  | { case: 'text', instructions: JSX.Element, program: string }

const do_shuffle = false;

function shuffleArray<A>(array: A[]): A[] {
  const shuffledArray = [];
  const originalArray = [...array];

  while (originalArray.length > 0) {
    const randomIndex = Math.floor(Math.random() * originalArray.length);
    shuffledArray.push(originalArray[randomIndex]);
    originalArray.splice(randomIndex, 1);
  }

  return shuffledArray;
}

function App() {
  useEffect(() => {
    console.log("[pantograph-user-study]")
  })

  const [exercises, set_exercises] = useState<Exercise[] | undefined>(undefined);

  useEffect(() => {
    const biexercises = do_shuffle ? shuffleArray(all_biexercises) : all_biexercises;
    const start_with_pantograph = Math.round(Math.random());
    console.log(JSON.stringify({ start_with_pantograph }));
    const mode = 'pantograph' as 'mixed' | 'pantograph' | 'text';
    switch (mode) {
      case 'mixed': {
        if (start_with_pantograph === 1) {
          set_exercises(biexercises.map((ex, i) => i < biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
        } else {
          set_exercises(biexercises.map((ex, i) => i >= biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
        }
        break;
      }
      case 'text': {
        set_exercises(biexercises.map((ex) => ({ case: 'text', instructions: ex.instructions, program: ex.text_program })));
        break;
      }
      case 'pantograph': {
        set_exercises(biexercises.map((ex) => ({ case: 'pantograph', instructions: ex.instructions })));
        break;
      }
    }
  }, []);

  const [exercise_status, set_exercise_status] = useState<'begin' | number | 'end'>('begin');
  // const [exercise_status, set_exercise_status] = useState<'begin' | number | 'end'>(0);

  function renderCurrentExercise(): JSX.Element {
    if (typeof exercise_status === 'number') {
      if (exercises === undefined) return renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `exercises === undefined`"}</div>)
      else if (!(0 <= exercise_status && exercise_status < exercises.length)) renderInstruction(<div>{"BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`"}</div>)
      else {
        const exercise = exercises[exercise_status];
        switch (exercise.case) {
          case 'text': {
            return renderInstructionAndExercise(
              exercise.instructions,
              (<Demonaco key={exercise_status} start_program={exercise.program} />)
            )
          }
          case 'pantograph': {
            return renderInstructionAndExercise(
              exercise.instructions,
              // (<Demonaco start_program={exercise.program} />)
              <iframe
                key={exercise_status}
                style={{
                  width: "100%",
                  height: "100%",
                }}
                // src={`${pantograph_url}?program=${exercise.program}`}
                src={`${pantograph_url}?UserStudyProgramIndex=${exercise_status}`}
              />
            )
          }
        }
      }
    } else {
      switch (exercise_status) {
        case 'begin': return renderBegin();
        case 'end': return renderEnd();
      }
    }

    throw new Error("impossible")
  }

  function renderControls(): JSX.Element {
    return (
      <div
        style={{
          padding: "0.5em",
          display: "flex",
          flexDirection: "row",
          gap: "0.5em"
        }}
      >
        <button onClick={(event) => {
          set_exercise_status(i => {
            const next_exercise_status = (() => {
              switch (i) {
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
        }}>
          next
        </button>
      </div>
    )
  }

  const renderBegin = () =>
  (
    <div
      style={{
        padding: "0.5em",
        display: 'flex',
        flexDirection: 'column',
        gap: "0.5em",
        width: "45em",
      }}
    >
      <div>
        <div>
          Welcome to the exercise section of the Pantograph user study.
          In this section, you will be presented with {exercises?.length} programming questions.
          For half of the questions you will use a text editor and for the other half you will use Pantograph.
          You will screen-record your session.
          <br></br>
          <br></br>
          You may ask the users study hosts any questions about the programming language, editors, or what a question is asking.
          However, they can't help you to solve the question.
          <br></br>
          <br></br>
          Once the study's hosts have announced that you may begin, press the start button.
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


  const renderEnd = () =>
  (
    <div
      style={{
        backgroundColor: "lightgreen",
        padding: "1em",
        width: "45em",
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
        display: "flex",
        flexDirection: "column",
      }}>
      <div
        style={{
          flexGrow: 0,
          flexShrink: 0,
        }}
      >
        {renderInstruction(<div style={{ fontSize: "1.5em" }}>Pantograph User Study</div>)}
        {renderControls()}
      </div>
      <div
        style={{
          flexGrow: 1,
          flexShrink: 0,
        }}
      >
        {renderCurrentExercise()}
      </div>
    </div>
  )
}

export default App

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

function renderInstructionAndExercise(instruction_body: JSX.Element, exercise_body: JSX.Element) {
  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        height: "100%",
      }}>
      <div
        style={{
          flexGrow: 0,
          flexShrink: 0
        }}
      >
        {renderInstruction(instruction_body)}
      </div>
      <div
        style={{
          flexGrow: 1,
          flexShrink: 0
        }}
      >
        {exercise_body}
      </div>
    </div>
  )
}

const renderCodeblock = (text: string) => (
  <div style={{ padding: "0.5em" }}>
    <code style={{ whiteSpace: "pre" }}>{text}</code>
  </div>
)

export const all_biexercises: BiExercise[] = [
  {
    instructions: (<span><i>(Do not copy text from these instructions to your clipboard)</i> Transcribe the following program into your editor:
      {renderCodeblock(
        [
					"let f : Int -> Int = fun x : Int => 5 / x in",
					"let m : Int = n + 1 in",
					"let y : Int = f m in",
					"y / n"
				].join("\n")
      )}
      Then, edit the program to result in this (swap the order of the definitions of <code>f</code> and <code>y</code>):
      {renderCodeblock(
        [
					"let y : Int =",
					"	let f : Int -> Int = fun x : Int => 5 / x in",
					"	let m : Int = n + 1 in",
					"	f m in",
					"y / n"
				].join("\n")
      )}
    </span>),
    text_program: "",
  },
  {
    instructions: (<span><i>(Do not copy text from these instructions to your clipboard)</i> Transcribe the following program into your editor:
      {renderCodeblock(
        [
          "let f : Int -> Int -> Int -> Int = fun x : Int => fun y : Int => fun z : Int => ? in",
					"let g : Int -> Int = fun x : Int => ? in",
					"let h : Int -> Int = fun x : int => ? in",
					"(f (g (h 1)) 2 3)"
				]
					.join("\n")
      )}
      Then, edit the program to result in this:
      {renderCodeblock(
        [
          "let f : Int -> Int = fun x : ?",
          "let g : Int -> Int = ?",
          "let h : Int -> Int -> Int -> Int = ? in",
          "(f (g (h 1)) 2 3)"
        ]
          .join("\n")
      )}
    </span>),
    text_program: "",
  },
  {
    instructions: (<span>You have been provided with an <i>incorrect</i> implementation of <code>deMorgansLaw</code>, which should be a function that takes as input two <code>Bool</code>s and output whether or not DeMorgan&apos;s Law holds for the inputs. Recall that DeMorgan&apos;s Law states that
      {renderCodeblock("!(p && q) == !p || !q")}
      Edit <code>deMorgansLaw</code> to be correct.</span>),
    text_program: `let deMorgansLaw : Bool -> Bool -> Bool =
    fun p => fun q =>
      p && q == p || q
      in

      deMorgansLaw true true &&
      deMorgansLaw true false &&
      deMorgansLaw false true &&
      deMorgansLaw false false`,
  },
  {
    instructions: (<span>
      You have been provided wth a <i>buggy</i> implementation of <code>collatz</code>, which should be a function that take sas input an integer <code>n : Int</code> and outputs the number of steps there are in the Collatz sequence starting from <code>n</code>.
      Recall that the Collatz sequence is defined as follows: given an element of the Collatz sequence <code>n</code>, the next element of the Collatz sequence is:
      <ul>
        <li>if <code>n</code> is <code>1</code>, then this is the end of the Collatz sequence</li>
        <li>if <code>n</code> is even, then <code>n / 2</code></li>
        <li>if <code>n</code> is odd, then <code>n * 3 + 1</code></li>
      </ul>

    </span>),
    text_program: `let collatz : Int -> Int =
    fun n =>
      if n == 1
      then 0
      else if (n % 2) == 0
      then collatz (n / 2)
      else collatz (n + 1) * 3

      in

      collatz 16`,
  },
  {
    instructions: (<span>You have been provided with a <i>partial</i> implementation of <code>isPrime</code>, which should be a function that takes as input an integer <code>n : Int</code> and outputs whether or not <code>n</code> is a prime. Finish implementing <code>isPrime</code> and fix any bugs.</span>),
    text_program: `let isPrime : Int -> Bool =
    let helper : Int -> Bool =
        fun x =>
      if x == 1
      then false
      else if (? % x) == 0
      then true
      else helper (x - 1) in
    fun n => not (helper (n - 1))
      in

      not (isPrime 1) &&
      isPrime 2 &&
      isPrime 3 &&
      not (isPrime 4) &&
      isPrime 5 &&
      not (isPrime 6) &&
      isPrime 7 &&
      not (isPrime 8)
      `,
  },
  {
    instructions: (<span>You have been provided with a stub for <code>reverse</code>, which should be a function that takes as input a list <code>ls : List Int</code> and output a <code>List Int</code> which has all the same elements as <code>ls</code> but in reversed order.</span>),
    text_program: `let reverse : List Int -> List Int =
      ?
      in

      reverse (cons 1 (cons 2 (cons 3 (cons 4 nil))))`,
  },
  {
    instructions: (<span>You have been provided with a stub for <code>filter</code>, which should be a function that takes as input a condition <code>cond : Int -&gt; Bool</code> and a list <code>ls : List Int</code>, and output a <code>List Int</code> which is the same as <code>ls</code> except without each element <code>x</code> such that <code>not (cond x)</code>.</span>),
    text_program: `let filter : (Int -> Bool) -> List Int -> List Int =
      ?
      in

filter (fun x => (x % 2) == 0) (cons 1 (cons 2 (cons 3 (cons 4 nil))))`,
  },
  {
    instructions: (
      <span>
        You have been provided with an implementation of <code>fold</code>, which is a function that folds over a <code>List</code> of <code>Int</code>s to produce an <code>Int</code> result.
        You have also been provided with a stub for a function <code>sum</code>, which should compute the sum of a <code>List</code> of <code>Int</code>s.
        Implement <code>sum</code> by using <code>fold</code>.
        Note that, by using <code>fold</code> correctly, you will <i>not</i> need to <code>match</code> on the input <code>List</code>.
      </span>
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

      sum (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))`
  },
  {
    instructions: (
      <span>
        You have been provided with an implementation of <code>sum</code>, which is a function that computes the sum of the elements of a <code>List Int</code>.
        Edit the implementation of <code>sum</code> so that it is <i>tail recursive</i>.
        That is, edit <code>sum</code> so that it takes an additional argument first, the <i>accumulator</i>, and computes the sum by passing an updated accumulator value to each recursive call such that at the end of the list (when <code>sum</code> is given <code>nil</code>) the accumulator&apos;s value is the sum of the list.
      </span>
    ),
    text_program: `let sum : List Int -> Int = 
    fun l =>
      match l with
        | nil => 0
        | cons h t => h + sum t
      in

      sum (cons 1 (cons 2 (cons 3 (cons 4 nil))))`
  },
]