import Head from "next/head";
import Image from "next/image";
import { Inter } from "next/font/google";
import styles from "@/styles/Home.module.css";
import { MouseEventHandler, useEffect, useState } from "react";
import * as Ex from './exercises';
import assert from "assert";

const do_shuffle = false;

const text_editor_url = "http://localhost:3000/"
const pantograph_url = "http://localhost:8000/"

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

export default function Home() {
	const [exercises, set_exercises] = useState<Ex.Exercise[] | undefined>(undefined);

	useEffect(() => {
		const biexercises = do_shuffle ? shuffleArray(Ex.all_biexercises) : Ex.all_biexercises;
		const start_with_pantograph = Math.round(Math.random());
		console.log(JSON.stringify({ start_with_pantograph }));
		if (start_with_pantograph === 1) {
			set_exercises(biexercises.map((ex, i) => i < biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions, program: ex.pantograph_program } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
		} else {
			set_exercises(biexercises.map((ex, i) => i >= biexercises.length / 2 ? { case: 'pantograph', instructions: ex.instructions, program: ex.pantograph_program } : { case: 'text', instructions: ex.instructions, program: ex.text_program }));
		}
	}, []);

	const [exercise_ix, set_exercise_ix] = useState<'loading' | 'begin' | number | 'end'>('begin');

	const next_exercise: MouseEventHandler<HTMLButtonElement> = () => {
		set_exercise_ix(i => {
			switch (i) {
				case 'loading': return 'loading';
				case 'begin': return 0;
				case 'end': return 'end';
				default: {
					if (exercises === undefined) { throw new Error("invariant violated: exercise_ix: number ==> exercises !== undefined"); }
					return i == exercises.length - 1 ? 'end' : i + 1
				}
			}
		})
	}

	return (
		<>
			<Head>
				<title>pantograph-user-study</title>
				<meta name="viewport" content="width=device-width, initial-scale=1" />
				<link rel="icon" href="/favicon.ico" />
			</Head>
			<main
				style={{
					padding: "1em",
					display: "flex", flexDirection: "column", gap: "1em",
					width: "calc(100vw - 2em)",
					height: "calc(100vh - 2em)",
				}}
			>
				<div
					style={{
						fontSize: "2em",
						fontWeight: "bold",
					}}
				>
					the Pantograph user study
				</div>
				{
					(() => {
						switch (exercise_ix) {
							case 'loading': {
								return (
									<div>loading exercises...</div>
								)
							}
							case 'begin': {
								return (
									<div
										style={{
											display: 'flex',
											flexDirection: 'column',
											gap: "0.5em",
											width: "45em",
										}}
									>
										<div>
											<div>
												Welcome to the exercise section of the Pantograph user study.
												In this section, you will be presented with {exercises?.length} programming questions that will involve transcribing, writing, and editing programs. 
												For half of the questions you will use a text editor and for the other half you will use Pantograph.
												You will screen-record your session.
												You may raise your hand in order to ask a question at any time during your session, but please keep the following in mind:
											</div>
											<ul>
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
												<li>This study's hosts have announced that you may begin this section (the exercise section) of this study.</li>
												<li>You have begun screen recording.</li>
											</ul>
										</div>
										<div>
											<button onClick={next_exercise}>start</button>
										</div>
									</div>
								);
							}
							case 'end': {
								return (
									<div
										style={{
											backgroundColor: "lightgreen",
											padding: "1em",
											width: "45em",
										}}
									>
										<div>
											<b>You're done with the exercise section of this study!</b>
										</div>
										<div>
											Please end your screen recording.
										</div>
									</div>
								);
							}
							default: {
								if (exercises === undefined) { throw new Error("invariant violated: exercise_ix: number ==> exercises !== undefined"); }
								const exercise = exercises[exercise_ix] as Ex.Exercise;
								let instructions: JSX.Element = (
									<div
										style={{
											width: "45em",
											display: "flex",
											gap: "0.5em",
											flexDirection: "column",
											userSelect: "none",
										}}
									>
										<div
											style={{
												display: "inline-block",
												backgroundColor: "black",
												color: "white",
												padding: "0.2em",
											}}
										>
											<b>Exercise {exercise_ix + 1} out of {exercises.length}</b>
										</div>
										<div>
											<b>Instructions.</b> {exercise.instructions}
										</div>
									</div>
								)
								let next_exercise_button: JSX.Element = (
									<div >
										<button onClick={next_exercise}>next exercise</button>
									</div>
								)
								switch (exercise.case) {
									case 'pantograph':
										return (
											<div
												style={{
													flexGrow: "1",
													display: 'flex',
													flexDirection: 'column',
													gap: "0.5em",
												}}
											>
												{instructions}
												{next_exercise_button}
												<iframe
													style={{
														width: "50em",
														height: "45em",
													}}
													src={`${pantograph_url}/?program=${exercise.program}`}
												/>
											</div>
										)
									case 'text':
										return (
											<div
												style={{
													flexGrow: "1",
													display: 'flex',
													flexDirection: 'column',
													gap: "0.5em",
												}}
											>
												{instructions}
												{next_exercise_button}
												<iframe
													src={`${text_editor_url}/?program=${exercise.program}`}
													style={{
														width: "50em",
														height: "45em",
													}}
												/>
											</div>
										)

								}
							}
						}
					})()
				}
			</main>
		</>
	);
}
