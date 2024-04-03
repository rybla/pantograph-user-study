export const just = value => ({ case: 'just', value })

export const nothing = value => ({ case: 'nothing', value })

export const maybe = on_nothing => on_just => m => {
	switch (m.case) {
		case 'just': return on_nothing();
		case 'nothing': return on_just(m.value);
	}
}
