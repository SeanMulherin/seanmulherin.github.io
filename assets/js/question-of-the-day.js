(function () {
	'use strict';

	var API_URL = 'https://datasets-server.huggingface.co/rows';
	var DAY_MS = 86400000;
	var CACHE_VERSION = 1;
	var CATEGORY_PATTERN = [
		'mathematics',
		'physics',
		'computer-science',
		'general-science',
		'statistics',
		'general-science',
		'mathematics',
		'physics',
		'computer-science',
		'general-science'
	];

	var SOURCES = {
		'mathematics': [
			mmluSource('high_school_mathematics', 270, 'High School Mathematics'),
			mmluSource('college_mathematics', 100, 'College Mathematics')
		],
		'physics': [
			mmluSource('high_school_physics', 151, 'High School Physics'),
			mmluSource('college_physics', 102, 'College Physics'),
			mmluSource('conceptual_physics', 235, 'Conceptual Physics')
		],
		'statistics': [
			mmluSource('high_school_statistics', 216, 'Statistics')
		],
		'computer-science': [
			mmluSource('high_school_computer_science', 100, 'Computer Science'),
			mmluSource('college_computer_science', 100, 'College Computer Science'),
			mmluSource('machine_learning', 112, 'Machine Learning')
		],
		'general-science': [
			arcSource('ARC-Challenge', 'train', 1119),
			arcSource('ARC-Challenge', 'validation', 299),
			arcSource('ARC-Challenge', 'test', 1172),
			arcSource('ARC-Easy', 'train', 2251),
			arcSource('ARC-Easy', 'validation', 570),
			arcSource('ARC-Easy', 'test', 2376)
		]
	};

	var FALLBACK_QUESTIONS = [
		{
			subject: 'Mathematics',
			question: 'A fair six-sided die is rolled twice. What is the probability that the sum is 7?',
			choices: ['1/12', '1/9', '1/6', '1/3'],
			answerIndex: 2
		},
		{
			subject: 'Physics',
			question: 'If an object moves at constant velocity, which statement must be true?',
			choices: ['Its acceleration is zero', 'No forces act on it', 'Its speed is increasing', 'Its kinetic energy is zero'],
			answerIndex: 0
		},
		{
			subject: 'Statistics',
			question: 'Which statistic is least affected by a single extreme outlier?',
			choices: ['Mean', 'Median', 'Range', 'Standard deviation'],
			answerIndex: 1
		},
		{
			subject: 'Computer Science',
			question: 'What is the worst-case time complexity of binary search on a sorted array?',
			choices: ['O(1)', 'O(log n)', 'O(n)', 'O(n log n)'],
			answerIndex: 1
		},
		{
			subject: 'General Science',
			question: 'Which process changes a liquid into a gas at the liquid surface?',
			choices: ['Condensation', 'Freezing', 'Evaporation', 'Deposition'],
			answerIndex: 2
		}
	];

	var elements = {
		date: document.getElementById('qod-date'),
		status: document.getElementById('qod-status'),
		form: document.getElementById('qod-form'),
		subject: document.getElementById('qod-subject'),
		question: document.getElementById('qod-question'),
		choices: document.getElementById('qod-choices'),
		submit: document.getElementById('qod-submit'),
		result: document.getElementById('qod-result'),
		resultTitle: document.getElementById('qod-result-title'),
		resultAnswer: document.getElementById('qod-result-answer'),
		source: document.getElementById('qod-source')
	};

	var today = utcToday();
	var activeQuestion = null;

	elements.date.textContent = today.date.toLocaleDateString('en-US', {
		weekday: 'long',
		year: 'numeric',
		month: 'long',
		day: 'numeric',
		timeZone: 'UTC'
	});

	loadQuestion().then(renderQuestion).catch(showLoadError);

	function mmluSource(config, count, subject) {
		return {
			dataset: 'cais/mmlu',
			config: config,
			split: 'test',
			count: count,
			subject: subject,
			format: 'mmlu',
			sourceName: 'MMLU',
			sourceUrl: 'https://huggingface.co/datasets/cais/mmlu',
			license: 'MIT'
		};
	}

	function arcSource(config, split, count) {
		return {
			dataset: 'allenai/ai2_arc',
			config: config,
			split: split,
			count: count,
			subject: 'General Science',
			format: 'arc',
			sourceName: 'AI2 ARC',
			sourceUrl: 'https://huggingface.co/datasets/allenai/ai2_arc',
			license: 'CC BY-SA 4.0'
		};
	}

	function utcToday() {
		var now = new Date();
		var midnight = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate()));
		return {
			date: midnight,
			key: midnight.toISOString().slice(0, 10),
			ordinal: Math.floor(midnight.getTime() / DAY_MS)
		};
	}

	function loadQuestion() {
		var cacheKey = questionCacheKey(today.key);
		var cached = readStorage(cacheKey);

		if (cached && isValidQuestion(cached)) {
			return Promise.resolve(cached);
		}

		var selection = selectDailyRow(today.ordinal);
		var params = new URLSearchParams({
			dataset: selection.source.dataset,
			config: selection.source.config,
			split: selection.source.split,
			offset: String(selection.offset),
			length: '1'
		});

		return fetch(API_URL + '?' + params.toString(), {
			headers: { Accept: 'application/json' }
		})
			.then(function (response) {
				if (!response.ok) {
					throw new Error('Question service returned ' + response.status);
				}
				return response.json();
			})
			.then(function (payload) {
				var rawRow = payload && payload.rows && payload.rows[0] && payload.rows[0].row;
				if (!rawRow) {
					throw new Error('Question row was unavailable');
				}
				var question = normalizeQuestion(rawRow, selection.source);
				writeStorage(cacheKey, question);
				return question;
			})
			.catch(function () {
				var fallback = fallbackQuestion(today.ordinal);
				writeStorage(cacheKey, fallback);
				return fallback;
			});
	}

	function selectDailyRow(dayOrdinal) {
		var patternIndex = dayOrdinal % CATEGORY_PATTERN.length;
		var category = CATEGORY_PATTERN[patternIndex];
		var pool = SOURCES[category];
		var total = pool.reduce(function (sum, source) { return sum + source.count; }, 0);
		var occurrence = categoryOccurrence(dayOrdinal, category);
		var start = hashString(category + '-offset-v1') % total;
		var step = coprimeStep(total, hashString(category + '-step-v1'));
		var poolIndex = (start + (occurrence % total) * step) % total;

		for (var i = 0; i < pool.length; i += 1) {
			if (poolIndex < pool[i].count) {
				return { source: pool[i], offset: poolIndex };
			}
			poolIndex -= pool[i].count;
		}

		throw new Error('Daily question selection failed');
	}

	function categoryOccurrence(dayOrdinal, category) {
		var cycleLength = CATEGORY_PATTERN.length;
		var fullCycles = Math.floor(dayOrdinal / cycleLength);
		var occurrencesPerCycle = CATEGORY_PATTERN.filter(function (item) { return item === category; }).length;
		var remainder = dayOrdinal % cycleLength;
		var earlierInCycle = CATEGORY_PATTERN.slice(0, remainder).filter(function (item) { return item === category; }).length;
		return fullCycles * occurrencesPerCycle + earlierInCycle;
	}

	function coprimeStep(total, seed) {
		var step = (seed % (total - 1)) + 1;
		while (greatestCommonDivisor(step, total) !== 1) {
			step = (step + 1) % total;
			if (step === 0) {
				step = 1;
			}
		}
		return step;
	}

	function greatestCommonDivisor(a, b) {
		while (b !== 0) {
			var remainder = a % b;
			a = b;
			b = remainder;
		}
		return a;
	}

	function hashString(value) {
		var hash = 2166136261;
		for (var i = 0; i < value.length; i += 1) {
			hash ^= value.charCodeAt(i);
			hash = Math.imul(hash, 16777619);
		}
		return hash >>> 0;
	}

	function normalizeQuestion(row, source) {
		var normalized;

		if (source.format === 'mmlu') {
			normalized = {
				question: String(row.question || '').trim(),
				choices: Array.isArray(row.choices) ? row.choices.map(String) : [],
				answerIndex: Number(row.answer)
			};
		} else {
			var choices = row.choices || {};
			var labels = Array.isArray(choices.label) ? choices.label.map(String) : [];
			var answerKey = String(row.answerKey || '').trim().toUpperCase();
			normalized = {
				question: String(row.question || '').trim(),
				choices: Array.isArray(choices.text) ? choices.text.map(String) : [],
				answerIndex: labels.map(function (label) { return label.trim().toUpperCase(); }).indexOf(answerKey)
			};
		}

		normalized.version = CACHE_VERSION;
		normalized.subject = source.subject;
		normalized.sourceName = source.sourceName;
		normalized.sourceUrl = source.sourceUrl;
		normalized.license = source.license;
		normalized.isFallback = false;

		if (!isValidQuestion(normalized)) {
			throw new Error('Question data was invalid');
		}
		return normalized;
	}

	function fallbackQuestion(dayOrdinal) {
		var fallback = FALLBACK_QUESTIONS[dayOrdinal % FALLBACK_QUESTIONS.length];
		return {
			version: CACHE_VERSION,
			subject: fallback.subject,
			question: fallback.question,
			choices: fallback.choices.slice(),
			answerIndex: fallback.answerIndex,
			sourceName: 'Local fallback question',
			sourceUrl: '',
			license: '',
			isFallback: true
		};
	}

	function isValidQuestion(question) {
		return question &&
			question.version === CACHE_VERSION &&
			typeof question.question === 'string' && question.question.length > 0 &&
			Array.isArray(question.choices) && question.choices.length >= 2 &&
			Number.isInteger(question.answerIndex) &&
			question.answerIndex >= 0 && question.answerIndex < question.choices.length;
	}

	function renderQuestion(question) {
		activeQuestion = question;
		elements.status.hidden = true;
		elements.form.hidden = false;
		elements.subject.textContent = question.subject;
		elements.question.textContent = question.question;
		elements.choices.textContent = '';

		question.choices.forEach(function (choice, index) {
			var wrapper = document.createElement('div');
			var input = document.createElement('input');
			var label = document.createElement('label');
			var letter = document.createElement('span');
			var text = document.createElement('span');

			wrapper.className = 'qod-choice';
			wrapper.dataset.index = String(index);
			input.type = 'radio';
			input.name = 'qod-answer';
			input.id = 'qod-choice-' + index;
			input.value = String(index);
			label.htmlFor = input.id;
			letter.className = 'qod-choice-letter';
			letter.textContent = String.fromCharCode(65 + index) + '.';
			text.textContent = choice;
			label.appendChild(letter);
			label.appendChild(text);
			wrapper.appendChild(input);
			wrapper.appendChild(label);
			elements.choices.appendChild(wrapper);

			input.addEventListener('change', function () {
				elements.submit.disabled = false;
			});
		});

		renderSource(question);
		var storedAnswer = readStorage(answerCacheKey(today.key));
		if (storedAnswer && Number.isInteger(storedAnswer.selectedIndex)) {
			var storedInput = elements.form.querySelector('input[value="' + storedAnswer.selectedIndex + '"]');
			if (storedInput) {
				storedInput.checked = true;
				revealAnswer(storedAnswer.selectedIndex, false);
			}
		}

		typesetMath();
	}

	function renderSource(question) {
		elements.source.textContent = '';
		elements.source.hidden = false;

		if (question.isFallback) {
			elements.source.textContent = 'Offline question from this site.';
			return;
		}

		elements.source.appendChild(document.createTextNode('Source: '));
		var link = document.createElement('a');
		link.href = question.sourceUrl;
		link.target = '_blank';
		link.rel = 'noopener noreferrer';
		link.textContent = question.sourceName;
		elements.source.appendChild(link);
		elements.source.appendChild(document.createTextNode(' via the Hugging Face Dataset Viewer API. License: ' + question.license + '.'));
	}

	elements.form.addEventListener('submit', function (event) {
		event.preventDefault();
		var selected = elements.form.querySelector('input[name="qod-answer"]:checked');
		if (!selected || !activeQuestion) {
			return;
		}

		var selectedIndex = Number(selected.value);
		writeStorage(answerCacheKey(today.key), { selectedIndex: selectedIndex });
		revealAnswer(selectedIndex, true);
	});

	function revealAnswer(selectedIndex, shouldFocus) {
		var isCorrect = selectedIndex === activeQuestion.answerIndex;
		var choiceWrappers = elements.choices.querySelectorAll('.qod-choice');

		choiceWrappers.forEach(function (wrapper) {
			var index = Number(wrapper.dataset.index);
			var input = wrapper.querySelector('input');
			input.disabled = true;
			if (index === activeQuestion.answerIndex) {
				wrapper.classList.add('is-correct');
			}
			if (index === selectedIndex && !isCorrect) {
				wrapper.classList.add('is-incorrect');
			}
		});

		elements.submit.disabled = true;
		elements.submit.hidden = true;
		elements.result.hidden = false;
		elements.result.classList.toggle('is-incorrect', !isCorrect);
		elements.resultTitle.textContent = isCorrect ? 'Correct' : 'Not quite';
		elements.resultAnswer.textContent = 'Correct answer: ' + activeQuestion.choices[activeQuestion.answerIndex];
		typesetMath();

		if (shouldFocus) {
			elements.result.focus();
		}
	}

	function typesetMath() {
		if (window.MathJax && typeof window.MathJax.typesetPromise === 'function') {
			window.MathJax.typesetPromise([elements.form, elements.result]).catch(function () {});
		}
	}

	function showLoadError() {
		elements.status.textContent = "Today's question could not be loaded. Please refresh and try again.";
	}

	function questionCacheKey(dateKey) {
		return 'qod-question-v' + CACHE_VERSION + ':' + dateKey;
	}

	function answerCacheKey(dateKey) {
		return 'qod-answer-v' + CACHE_VERSION + ':' + dateKey;
	}

	function readStorage(key) {
		try {
			var value = window.localStorage.getItem(key);
			return value ? JSON.parse(value) : null;
		} catch (error) {
			return null;
		}
	}

	function writeStorage(key, value) {
		try {
			window.localStorage.setItem(key, JSON.stringify(value));
		} catch (error) {
			// The app remains usable when storage is unavailable.
		}
	}
}());
