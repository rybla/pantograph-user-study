import { useEffect, useRef, useState } from "react";
import React, { JSX } from "react";
import Demonaco from "./Demonaco";

const pantograph_url = "./pantograph.html";
const mode = "mixed" as "mixed" | "pantograph" | "text";
const check_tick = 500;

export type BiExercise = {
  instructions: JSX.Element;
  text_program: string;
  pantograph_program_index: string;
  expected_output: string;
};
export type Exercise = { case: "pantograph" | "text" } & BiExercise;

export default function App({ debug }: { debug?: boolean }) {
  function log(...msg: any) {
    if (debug) console.log(...msg);
  }

  useEffect(() => {
    log("[pantograph-user-study]");
    log(JSON.stringify({ debug }));
  });

  const [exercises, set_exercises] = useState<Exercise[] | undefined>(
    undefined,
  );

  function initExercises(start_with_pantograph: boolean) {
    switch (mode) {
      case "mixed": {
        if (start_with_pantograph) {
          set_exercises(
            all_biexercises.map((ex, i) =>
              i < all_biexercises.length / 2
                ? { case: "pantograph", ...ex }
                : { case: "text", ...ex },
            ),
          );
        } else {
          set_exercises(
            all_biexercises.map((ex, i) =>
              i >= all_biexercises.length / 2
                ? { case: "pantograph", ...ex }
                : { case: "text", ...ex },
            ),
          );
        }
        break;
      }
      case "text": {
        set_exercises(all_biexercises.map((ex) => ({ case: "text", ...ex })));
        break;
      }
      case "pantograph": {
        set_exercises(
          all_biexercises.map((ex) => ({ case: "pantograph", ...ex })),
        );
        break;
      }
    }
  }

  function initGroupA() {
    initExercises(true);
  }

  function initGroupB() {
    initExercises(false);
  }

  // const [exercise_status, set_exercise_status] = useState<'text-tutorial' | 'begin' | number | 'end'>('begin');
  const [exercise_status, set_exercise_status] = useState<
    "text-tutorial" | "begin" | number | "end"
  >("text-tutorial");

  const [check_result, set_check_result] = useState<boolean | undefined>(
    undefined,
  );

  // for use in checking, need to have refs to the most recent version of exercise_status and exercises
  const exercise_status_ref = useRef<typeof exercise_status>("text-tutorial");
  const exercises_ref = useRef<typeof exercises>([]);
  useEffect(() => {
    exercise_status_ref.current = exercise_status;
    exercises_ref.current = exercises;
  }, [exercise_status, exercises]);

  useEffect(() => {
    const intervalId = setInterval(() => {
      const exercise_status = exercise_status_ref.current;
      const exercises = exercises_ref.current;
      log(`checking exercise_status ${exercise_status}`);
      if (typeof exercise_status === "number") {
        if (exercises === undefined)
          return console.error(
            "BUG: `typeof exercise_status === 'number'` but `exercises === undefined`",
          );
        else if (!(0 <= exercise_status && exercise_status < exercises.length))
          return console.error(
            "BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`",
          );
        const exercise = exercises[exercise_status];

        function checkText() {
          const evaluation = document
            .getElementById("evaluation")
            ?.innerText.trim();
          if (evaluation === undefined)
            throw new Error("#evaluation not found");
          log({ evaluation });
          if (evaluation === "") {
            set_check_result(undefined);
            return;
          }
          set_check_result(evaluation.trim() === exercise.expected_output);
        }

        function checkPantograph() {
          const iframe = document.getElementById(
            "pantograph-iframe",
          ) as HTMLIFrameElement;
          const evaluation =
            iframe?.contentWindow?.document.getElementById(
              "evaluation",
            )?.innerText;
          if (evaluation === undefined)
            throw new Error("#evaluation not found");
          log({ evaluation });
          if (evaluation === "") {
            set_check_result(undefined);
            return;
          }
          set_check_result(evaluation.trim() === exercise.expected_output);
        }

        switch (exercise.case) {
          case "pantograph": {
            checkPantograph();
            break;
          }
          case "text": {
            checkText();
            break;
          }
        }
      }
    }, check_tick);
    return () => clearInterval(intervalId);
  }, []);

  function renderCurrentInstruction(): JSX.Element {
    if (typeof exercise_status === "number") {
      if (exercises === undefined)
        return renderInstruction(
          <div>
            {
              "BUG: `typeof exercise_status === 'number'` but `exercises === undefined`"
            }
          </div>,
        );
      else if (!(0 <= exercise_status && exercise_status < exercises.length))
        return renderInstruction(
          <div>
            {
              "BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`"
            }
          </div>,
        );
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
    } else {
      switch (exercise_status) {
        case "text-tutorial":
          return renderTextTutorial();
        case "begin":
          return renderBegin();
        case "end":
          return renderEnd();
      }
    }

    return <></>;
  }

  function renderCurrentExercise(): JSX.Element {
    if (typeof exercise_status === "number") {
      if (exercises === undefined)
        return renderInstruction(
          <div>
            {
              "BUG: `typeof exercise_status === 'number'` but `exercises === undefined`"
            }
          </div>,
        );
      else if (!(0 <= exercise_status && exercise_status < exercises.length))
        renderInstruction(
          <div>
            {
              "BUG: `typeof exercise_status === 'number'` but `!(0 <= exercise_status && exercise_status < exercises.length)`"
            }
          </div>,
        );
      else {
        const exercise = exercises[exercise_status];
        switch (exercise.case) {
          case "text": {
            return (
              <Demonaco
                key={exercise_status}
                start_program={exercise.text_program}
              />
            );
          }
          case "pantograph": {
            return (
              <iframe
                key={exercise_status}
                id="pantograph-iframe"
                style={{
                  width: "100%",
                  height: "100%",
                  border: "none",
                  margin: "0",
                  padding: "0",
                }}
                src={`${pantograph_url}?UserStudyProgramIndex=${exercise.pantograph_program_index}`}
              />
            );
          }
        }
      }
    } else if (exercise_status === "text-tutorial") {
      return (
        <Demonaco
          key={exercise_status}
          start_program={`let a : Int = 1 in
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
`}
        />
      );
    }

    return <></>;
  }

  function renderControls(): JSX.Element {
    const nextExercise = () => {
      set_exercise_status((i) => {
        const next_exercise_status = (() => {
          set_check_result(undefined);
          switch (i) {
            case "text-tutorial":
              return "begin";
            case "begin":
              return 0;
            case "end":
              return "end";
            default: {
              if (exercises === undefined)
                throw new Error(
                  "invariant violated: exercise_ix: number ==> exercises !== undefined",
                );
              return i == exercises.length - 1 ? "end" : i + 1;
            }
          }
        })();
        log("next_exercise_ix", next_exercise_status);
        if (
          exercises !== undefined &&
          typeof next_exercise_status === "number"
        ) {
          log(
            "exercises[next_exercise_ix].case",
            exercises[next_exercise_status].case,
          );
        }
        return next_exercise_status;
      });
    };

    const prevExercise = () => {
      set_exercise_status((i) => {
        const prev_exercise_status = ((): typeof exercise_status => {
          switch (i) {
            case "text-tutorial":
              return "text-tutorial";
            case "begin":
              return "text-tutorial";
            case "end": {
              if (exercises === undefined)
                throw new Error(
                  "invariant violated: exercise_ix: 'end' ==> exercises !== undefined",
                );
              return exercises.length - 1;
            }
            default: {
              if (i === 0) return "begin";
              else return i - 1;
            }
          }
        })();
        log("prev_exercise_ix", prev_exercise_status);
        if (
          exercises !== undefined &&
          typeof prev_exercise_status === "number"
        ) {
          log(
            "exercises[prev_exercise_ix].case",
            exercises[prev_exercise_status].case,
          );
        }
        return prev_exercise_status;
      });
    };

    const renderContainer = (kids: JSX.Element[]) => (
      <div
        style={{
          padding: "0.5em",
          display: "flex",
          flexDirection: "row",
          gap: "0.5em",
        }}
      >
        {kids}
      </div>
    );

    switch (exercise_status) {
      case "text-tutorial":
        return renderContainer([<button onClick={nextExercise}>next</button>]);
      case "begin":
        return renderContainer([
          <button onClick={prevExercise}>back</button>,
          <button
            onClick={() => {
              initGroupA();
              nextExercise();
            }}
          >
            Begin Group A
          </button>,
          <button
            onClick={() => {
              initGroupB();
              nextExercise();
            }}
          >
            Begin Group B
          </button>,
        ]);
      case "end":
        return renderContainer([<button onClick={prevExercise}>back</button>]);
      default:
        return renderContainer([
          <button onClick={prevExercise}>back</button>,
          <button
            onClick={nextExercise}
            disabled={check_result !== true && !(debug === true)}
            style={{
              backgroundColor:
                check_result === undefined
                  ? "inherit"
                  : check_result === false
                    ? "pink"
                    : "lightgreen",
            }}
          >
            next
          </button>,
        ]);
    }
  }

  const renderTextTutorial = () => (
    <div
      style={{
        padding: "0.5em",
        display: "flex",
        flexDirection: "column",
        gap: "0.5em",
      }}
    >
      {renderExerciseTitle("Text Editor Introduction")}
      <div>
        To the left of these instructions is a text editor with the basic
        editing features of programming-focussed text editors such as VS Code,
        Sublime Text, etc. This text editor also has a few extra features:
        <ul>
          <li>Checks syntax automatically</li>
          <li>Checks types automatically</li>
          <li>Hover over errors for more info</li>
        </ul>
      </div>
      <div>
        The programming language used with the text editor is essentially the
        same as the one you used in the Pantograph tutorial, with a few
        variations:
        <ul>
          <li>A hole is written {renderCode("?")}</li>
          <li>Hover over a hole to see its expected type</li>
          <li>
            Type annotations are <i>optional</i>
          </li>
        </ul>
      </div>
      <div>
        Take a few minutes to get familiar with the text editor. For example:
        <ul>
          <li>
            Write a simple expression e.g. {renderCode("let x = 4 in ? x x")}
          </li>
          <li>Note syntax error feedback on hover</li>
          <li>Note type error feedback on hover</li>
          <li>Write a hole and view its type on hover</li>
        </ul>
      </div>
    </div>
  );

  const renderBegin = () => (
    <div
      style={{
        padding: "0.5em",
        display: "flex",
        flexDirection: "column",
        gap: "0.5em",
      }}
    >
      <div>
        <div>
          Welcome to the exercise section of the Pantograph user study. In this
          section, you will be presented with a selection of programming
          exercises. For half of the exercises you will use a text editor and
          for the other half you will use Pantograph. You will screen-record
          your session using{" "}
          <a href="https://recordscreen.io/" target="_blank">
            screenrecord.io
          </a>
          .{" "}
          <b>Make sure to test that screen recording works before you begin.</b>
          <br></br>
          <br></br>
          You may ask the users study hosts any questions about the programming
          language, editors, or exercise instructions. However, they can't help
          you to solve an exercise.
          <br></br>
          <br></br>
          Once the study's hosts have announced that you may begin, start screen
          recording and then press the "Begin" button above according to your
          assigned group.
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
  );

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
      <div>Please end your screen recording.</div>
    </div>
  );

  return (
    <div
      style={{
        height: "100vh",
        width: "100vw",
        display: "flex",
        flexDirection: "row-reverse",
      }}
    >
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
        >
          Pantograph User Study
        </div>
        {renderControls()}
        {renderCurrentInstruction()}
      </div>
      <div
        style={{
          flexGrow: 1,
          flexShrink: 0,
          boxShadow: "-1px 0 0 0 black inset",
        }}
      >
        {renderCurrentExercise()}
      </div>
    </div>
  );
}

const Button = (props: {
  onClick: React.MouseEventHandler<HTMLButtonElement>;
  children: JSX.Element[];
}) => {
  return (
    <button onClick={props.onClick} onFocus={(event) => event.target.blur()}>
      {props.children}
    </button>
  );
};

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
  );
}

const renderCodeblock = (text: string) => (
  <div
    style={{
      padding: "0.5em",
      margin: "0.5em 0",
      backgroundColor: "rgba(0, 0, 0, 0.1)",
      boxShadow: "1px 1px 4px 0 black",
    }}
  >
    <code style={{ whiteSpace: "pre" }}>{text}</code>
  </div>
);

const renderCode = (text: string) => (
  <code
    style={{
      padding: "0 0.2em",
      backgroundColor: "rgba(0, 0, 0, 0.1)",
    }}
  >
    {text}
  </code>
);

const renderExerciseTitle = (text: string) => (
  <div
    style={{
      fontSize: "1.2em",
      textDecoration: "underline",
      marginBottom: "0.5em",
    }}
  >
    {text}
  </div>
);

const renderParagraphs = (ps: JSX.Element[]) => (
  <div style={{ display: "flex", flexDirection: "column", gap: "0.5em" }}>
    {ps.map((p, i) => (
      <div key={i}>{p}</div>
    ))}
  </div>
);

const transcribe1: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("Transcribe and Edit")}
      <div>
        <i>
          (<b>Do not</b> copy text from these instructions)
        </i>
      </div>
      <div>
        Transcribe the following program into your editor. Whitespace does not
        have to be exact.
        {renderCodeblock(
          [
            "let f : Int -> Int = fun x : Int => 4 * x in",
            "let m : Int = 1 + 1 in",
            "let y : Int = f m in",
            "y / 2",
          ].join("\n"),
        )}
        Then, edit the program to result in this (Move the definitions of{" "}
        {renderCode("f")} and {renderCode("m")} inside the definition of{" "}
        {renderCode("y")}):
        {renderCodeblock(
          [
            "let y : Int =",
            "    let f : Int -> Int = fun x : Int => 4 * x in",
            "    let m : Int = 1 + 1 in",
            "    f m in",
            "y / 2",
          ].join("\n"),
        )}
      </div>
    </div>
  ),
  text_program: "",
  pantograph_program_index: "transcribe1",
  expected_output: "4",
};

const transcribe2: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("Transcribe and Edit")}
      <div>
        <i>
          (<b>Do not</b> copy text from these instructions)
        </i>
      </div>
      <div>
        Transcribe the following program into your editor. Whitespace does not
        have to be exact.
        {renderCodeblock(
          ["fun x : Int => ", "    let i : Int = 7 in", "    x / i"].join("\n"),
        )}
        Then, edit the program to result in this (move the function into the
        definition of a let expression):
        {renderCodeblock(
          [
            "let f : Int -> Int =",
            "    fun x : Int => ",
            "        let i : Int = 7 in",
            "        x / i in",
            "f 7",
          ].join("\n"),
        )}
      </div>
    </div>
  ),
  text_program: "",
  pantograph_program_index: "transcribe2",
  expected_output: "1",
};

const demorgan: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("DeMorgan's Law")}
      <div>
        You have been provided with an <i>incorrect</i> implementation of{" "}
        {renderCode("deMorgansLaw")}, which should be a function that takes as
        input two {renderCode("Bool")}s and output whether or not
        DeMorgan&apos;s Law holds for the inputs. Recall that DeMorgan&apos;s
        Law states that
        {renderCodeblock("!(p && q) == !p || !q")}
      </div>
      <br />
      <div>Edit {renderCode("deMorgansLaw")} to be correct.</div>
      <br />
      <div>Run should output {renderCode("true")}.</div>
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
  expected_output: "true",
};

const collatz: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("Collatz")}
      <div>
        You have been provided wth a <i>buggy</i> implementation of{" "}
        {renderCode("collatz")}, which should be a function that takes as input
        an integer {renderCode("n : Int")} and outputs the number of steps there
        are in the Collatz sequence starting from {renderCode("n")}. Recall that
        the Collatz sequence is defined as follows: given an element of the
        Collatz sequence {renderCode("n")}, the next element of the Collatz
        sequence is:
        <ul>
          <li>
            if {renderCode("n")} is {renderCode("1")}, then this is the end of
            the Collatz sequence
          </li>
          <li>
            if {renderCode("n")} is even, then {renderCode("n / 2")}
          </li>
          <li>
            if {renderCode("n")} is odd, then {renderCode("n * 3 + 1")}
          </li>
        </ul>
      </div>
      <br />
      <div>Edit {renderCode("collatz")} to be correct.</div>
      <br />
      <div>Run should output {renderCode("5")}.</div>
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
  expected_output: "5",
};

const prime: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("Prime")}
      <div>
        You have been provided with a <i>partial</i> implementation of{" "}
        {renderCode("isPrime")}, which should be a function that takes as input
        an integer {renderCode("n : Int")} and outputs whether or not{" "}
        {renderCode("n")} is a prime.
      </div>
      <br />
      <div>Finish implementing {renderCode("isPrime")} and fix any bugs.</div>
      <br />
      <div>Run should output {renderCode("true")}.</div>
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
  expected_output: "true",
};

const reverse: BiExercise = {
  instructions: (
    <div>
      {renderExerciseTitle("Reverse")}
      <div>
        You have been provided with a stub for {renderCode("reverse")}, which
        should be a function that takes as input a list{" "}
        {renderCode("ls : List Int")} and outputs the reversed{" "}
        {renderCode("List Int")} -- that is, the {renderCode("List Int")} which
        has all the same elements as {renderCode("ls")} but in reversed order.
      </div>
      <br />
      <div>
        Run should output {renderCode("(cons 4 (cons 3 (cons 2 (cons 1))))")}.
      </div>
      <br />
      <div>
        <i>Note.</i> There is a built-in function{" "}
        {renderCode("append : List Int -> List Int -> List Int")} which appends
        two {renderCode("List Int")}s together.
      </div>
      <br />
      <div>
        <i>Note.</i> You can match on a list {renderCode("ls")} with the
        following syntax:
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
  expected_output: "(cons 4 (cons 3 (cons 2 (cons 1))))",
};

const filter: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("Filter"),
    <>
      You have been provided with a stub for {renderCode("filter")}, which
      should be a function that takes as input a condition{" "}
      {renderCode("cond : Int -> Bool")} and a list{" "}
      {renderCode("ls : List Int")}, and output a {renderCode("List Int")} which
      is the same as {renderCode("ls")} except without each element{" "}
      {renderCode("x")} such that {renderCode("not (cond x)")}.
    </>,
    <>Run should output {renderCode("(cons 2 (cons 4 nil))")}.</>,
  ]),
  text_program: `let filter : (Int -> Bool) -> List Int -> List Int =
    ?
in

filter (fun x => (x % 2) == 0) (cons 1 (cons 2 (cons 3 (cons 4 nil))))
`,
  pantograph_program_index: "filter",
  expected_output: "(cons 2 (cons 4 nil)",
};

const filterWithIndex: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("Filter with Index"),
    <>
      You have been provided with a <i>correct</i> implementation of{" "}
      {renderCode("filter")}.
    </>,
    <>
      Edit {renderCode("filter")} to be {renderCode("filterWithIndex")}, which
      is the same as {renderCode("filter")} except the filter condition can also
      use the index of the element in the list.
    </>,
    <>In order to accomplish this, you must do the following edits:</>,
    <ol>
      <li>
        Rename {renderCode("filter")} to be {renderCode("filterWithIndex")}.
      </li>
      <li>
        Insert a new second input to {renderCode("filterWithIndex")} called{" "}
        {renderCode("i")} of type {renderCode("Int")}, which is the starting
        index.
      </li>
      <li>
        Change the {renderCode("filterWithIndex")}'s input type{" "}
        {renderCode("Bool -> Bool")} to be {renderCode("Int -> Bool -> Bool")},
        where the {renderCode("Int")} is the current index.
      </li>
      <li>
        Use the new input {renderCode("i")} as the first argument of{" "}
        {renderCode("cond")} in the condition of the {renderCode("if")}.
      </li>
      <li>
        Give {renderCode("i + 1")} as the new second argument to each recursive
        call to {renderCode("filterWithIndex")}.
      </li>
      <li>
        On the last line, insert a new first input to the second argument of{" "}
        {renderCode("filter")}, and use that input to fill in the hole of type{" "}
        {renderCode("Int")}.
      </li>
      <li>
        On the last line, give {renderCode("0")} as the new second argument to{" "}
        {renderCode("filterWithIndex")}.
      </li>
    </ol>,
    <>
      When you're done, the type of {renderCode("filterWithIndex")} should be
    </>,
    renderCodeblock("(Int -> Bool -> Bool) -> Int -> List Bool -> List Bool"),
    <>Run should output {renderCode("(cons true nil)")}.</>,
  ]),
  text_program: `let filter : (Bool -> Bool) -> List Bool -> List Bool =
    fun cond => fun l =>
        match l with
        | nil => nil
        | cons h t => if cond h then cons h (filter cond t) else filter cond t
in

filter
  (fun b => b && ((? % 2) == 0))
  (cons true (cons true (cons false (cons true nil))))
`,
  pantograph_program_index: "filterWithIndex",
  expected_output: "(cons true nil)",
};

const sumViaFold: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("Sum via Fold"),
    <>
      You have been provided with an implementation of {renderCode("fold")},
      which is a function that folds over a {renderCode("List Int")} to produce
      an {renderCode("Int")} result. You have also been provided with a stub for
      a function {renderCode("sum")}, which should compute the sum of a{" "}
      {renderCode("List Int")}. Implement {renderCode("sum")} by using{" "}
      {renderCode("fold")}. Note that, by using {renderCode("fold")} correctly,
      you will <i>not</i> need to {renderCode("match")} on the input{" "}
      {renderCode("List")}.
    </>,
    <>Run should output {renderCode("10")}.</>,
  ]),
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
  pantograph_program_index: "sumViaFold",
  expected_output: "10",
};

const sumFromViaFold: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("Sum From via Fold"),
    <>
      You have been provided with a <i>correct</i> implementation of{" "}
      {renderCode("sum")}. Edit {renderCode("sum")} to be{" "}
      {renderCode("sumFrom")}, which takes as input the starting amount add the
      sum of the list to. This requires the following edits:
      <ol>
        <li>
          Rename {renderCode("sum")} to {renderCode("sumFrom")}
        </li>
        <li>
          Delete the last argument of {renderCode("fold")} in the body of{" "}
          {renderCode("sumFrom")} and edit {renderCode("sumFrom")}'s type to be{" "}
          {renderCode("Int -> List Int -> Int")}
        </li>
        <li>
          Insert {renderCode("7")} as a new first argument of{" "}
          {renderCode("sumFrom")} on the last line.
        </li>
      </ol>
    </>,
    <>Run should output {renderCode("17")}.</>,
  ]),
  text_program: `let fold : (Int -> Int -> Int) -> Int -> List Int -> Int =
    fun f => fun n => fun ls =>
        match ls with
        | nil => n
        | cons h t => fold f (f n h) t
in

let sum : List Int -> Int =
  fold (fun x => fun y => x + y) 0
in

sum (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))`,
  pantograph_program_index: "sumFromViaFold",
  expected_output: "17",
};

const allEvenViaFold: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("All Even via Fold"),
    <>
      You have been provided with a <i>correct</i> implementation of{" "}
      {renderCode("fold")}, which is a function that folds over a{" "}
      {renderCode("List Int")} to produce an {renderCode("Int")} result.
    </>,
    <>
      You have also been provided for a stub for a function{" "}
      {renderCode("allEven")}, which should evaluate to {renderCode("true")} if
      the input list only contains even numbers, and {renderCode("false")}{" "}
      otherwise.
    </>,
    <>
      Implement {renderCode("allEven")} by using {renderCode("fold")}. Note
      that, by using {renderCode("fold")} correctly, you will not need to{" "}
      {renderCode("match")} on the input list.
    </>,
  ]),
  text_program: `let fold : (Bool -> Int -> Bool) -> Bool -> List Int -> Bool =
    fun f => fun n => fun l =>
        match l with
        | nil => n
        | cons h t => fold f (f n h) t
in

let isEven = fun x => (x % 2) == 0 in

let allEven : List Int -> Bool = ? in

allEven (cons 0 (cons 2 (cons 4 nil)))`,
  pantograph_program_index: "allEvenViaFold",
  expected_output: "true",
};

const allViaFold: BiExercise = {
  instructions: renderParagraphs([
    renderExerciseTitle("All via Fold"),
    <>
      You have been provided with a <i>correct</i> implementation of{" "}
      {renderCode("allEven")}, which checks whether all the numbers in the input
      list are even.
    </>,
    <>
      Edit {renderCode("allEven")} to be {renderCode("all")}, which is the same
      as {renderCode("allEven")} except it takes an arbitrary condition of type{" "}
      {renderCode("Int -> Bool")} as a new first input, and checks whether all
      the numbers in the input list satisfy that condition.
    </>,
    <>
      This requires the following edits:
      <ol>
        <li>
          Change the name of {renderCode("allEven")} to {renderCode("all")}.
        </li>
        <li>
          Insert a new input to {renderCode("all")} called {renderCode("cond")}{" "}
          of type {renderCode("Int -> Bool")}
        </li>
        <li>
          Replace {renderCode("isEven")} with {renderCode("cond")} in the body
          of {renderCode("all")}.
        </li>
        <li>
          On the last line, insert {renderCode("(fun x => (! (isEven x)))")} as
          a new second argument to {renderCode("all")}.
        </li>
      </ol>
    </>,
    <>
      When you're done the type of {renderCode("all")} should be
      {renderCodeblock("(Int -> Bool) -> List Int -> Bool")}
    </>,
    <>Run should output {renderCode("false")}.</>,
  ]),
  text_program: `let fold : (Bool -> Int -> Bool) -> Bool -> List Int -> Bool =
    fun f => fun n => fun l =>
        match l with
        | nil => n
        | cons h t => fold f (f n h) t
in

let isEven = fun x => (x % 2) == 0 in

let allEven : List Int -> Bool = fold (fun b => fun x => b && isEven x) true in

allEven (cons 0 (cons 2 (cons 4 nil)))`,
  pantograph_program_index: "allViaFold",
  expected_output: "true",
};

export const all_biexercises: BiExercise[] = [
  transcribe1,
  allEvenViaFold,
  allViaFold,
  sumViaFold,
  sumFromViaFold,
  // ----------
  demorgan,
  transcribe2,
  filter,
  filterWithIndex,
  reverse,
];
