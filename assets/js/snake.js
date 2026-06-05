(function() {
  var GRID_SIZE = 24;
  var INITIAL_SPEED_MS = 118;
  var MIN_SPEED_MS = 62;
  var SCORE_STEP = 10;
  var LEADERBOARD_LIMIT = 25;
  var SCORE_COLLECTION = 'snake_scores';
  var BEST_SCORE_KEY = 'portfolioSnakeBestScore';
  var MAX_SCORE = (GRID_SIZE * GRID_SIZE - 3) * SCORE_STEP;

  var firebaseConfig = {
    apiKey: 'AIzaSyB9DzGVDOIyC_Vh1BA_-Q6K3BUp_qJj_Ug',
    authDomain: 'portfolio-blog-f49c1.firebaseapp.com',
    databaseURL: 'https://portfolio-blog-f49c1-default-rtdb.firebaseio.com',
    projectId: 'portfolio-blog-f49c1',
    storageBucket: 'portfolio-blog-f49c1.firebasestorage.app',
    messagingSenderId: '884520998565',
    appId: '1:884520998565:web:47a9f889796c4347131f4c',
    measurementId: 'G-4K7ZPEPTYB'
  };

  var db = null;
  var canvas = null;
  var context = null;
  var nameInput = null;
  var startButton = null;
  var pauseButton = null;
  var refreshButton = null;
  var scoreElement = null;
  var bestElement = null;
  var stateElement = null;
  var messageElement = null;
  var leaderboardElement = null;

  var snake = [];
  var food = { x: 17, y: 12 };
  var direction = { x: 1, y: 0 };
  var nextDirection = { x: 1, y: 0 };
  var score = 0;
  var bestScore = 0;
  var speedMs = INITIAL_SPEED_MS;
  var running = false;
  var paused = false;
  var gameOver = false;
  var scoreSubmitted = false;
  var lastFrameTime = 0;
  var animationFrameId = null;
  var touchStart = null;

  function sanitizeName(value) {
    return String(value || '')
      .replace(/[^A-Za-z0-9 _.-]/g, '')
      .replace(/^[^A-Za-z0-9]+/, '')
      .replace(/\s+/g, ' ')
      .trim()
      .slice(0, 24);
  }

  function setMessage(message, isError) {
    if (!messageElement)
      return;

    messageElement.textContent = message || '';
    messageElement.classList.toggle('snake-message-error', !!isError);
  }

  function setState(label) {
    if (stateElement)
      stateElement.textContent = label;
  }

  function updateScoreboard() {
    if (scoreElement)
      scoreElement.textContent = String(score);

    if (bestElement)
      bestElement.textContent = String(bestScore);
  }

  function loadLocalBestScore() {
    var stored = Number(window.localStorage.getItem(BEST_SCORE_KEY) || 0);
    bestScore = Number.isFinite(stored) && stored > 0 ? Math.floor(stored) : 0;
    updateScoreboard();
  }

  function saveLocalBestScore() {
    if (score <= bestScore)
      return;

    bestScore = score;
    window.localStorage.setItem(BEST_SCORE_KEY, String(bestScore));
    updateScoreboard();
  }

  function initializeFirebase() {
    if (!window.firebase || !firebase.firestore)
      return null;

    if (!firebase.apps.length)
      firebase.initializeApp(firebaseConfig);

    return firebase.firestore();
  }

  function formatScoreDate(timestamp) {
    if (!timestamp || typeof timestamp.toDate !== 'function')
      return '';

    return timestamp.toDate().toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric'
    });
  }

  function renderLeaderboard(scores) {
    leaderboardElement.textContent = '';

    if (!scores.length) {
      var emptyItem = document.createElement('li');
      emptyItem.className = 'snake-empty-score';
      emptyItem.textContent = 'No scores yet.';
      leaderboardElement.appendChild(emptyItem);
      return;
    }

    scores.forEach(function(entry) {
      var item = document.createElement('li');
      var name = document.createElement('span');
      var scoreValue = document.createElement('strong');
      var date = document.createElement('small');

      name.className = 'snake-score-name';
      name.textContent = entry.name || 'Player';
      scoreValue.textContent = String(entry.score || 0);
      date.textContent = formatScoreDate(entry.createdAt);

      item.appendChild(name);
      item.appendChild(scoreValue);
      item.appendChild(date);
      leaderboardElement.appendChild(item);
    });
  }

  async function loadLeaderboard() {
    if (!leaderboardElement)
      return;

    if (!db) {
      renderLeaderboard([]);
      setMessage('Leaderboard is unavailable right now.', true);
      return;
    }

    leaderboardElement.textContent = '';
    var loadingItem = document.createElement('li');
    loadingItem.className = 'snake-empty-score';
    loadingItem.textContent = 'Loading scores...';
    leaderboardElement.appendChild(loadingItem);

    try {
      var snapshot = await db.collection(SCORE_COLLECTION)
        .orderBy('score', 'desc')
        .limit(LEADERBOARD_LIMIT)
        .get();
      var scores = [];

      snapshot.forEach(function(doc) {
        scores.push(doc.data());
      });

      renderLeaderboard(scores);
    } catch (error) {
      console.error('Error loading Snake leaderboard:', error);
      renderLeaderboard([]);
      setMessage('Leaderboard could not load.', true);
    }
  }

  async function submitScore() {
    var playerName = sanitizeName(nameInput.value);

    if (!db || scoreSubmitted || score <= 0 || !playerName)
      return;

    scoreSubmitted = true;
    setMessage('Submitting score...');

    try {
      await db.collection(SCORE_COLLECTION).add({
        name: playerName,
        score: Math.min(score, MAX_SCORE),
        createdAt: firebase.firestore.FieldValue.serverTimestamp()
      });
      setMessage('Score submitted.');
      await loadLeaderboard();
    } catch (error) {
      console.error('Error submitting Snake score:', error);
      setMessage('Score could not be submitted.', true);
    }
  }

  function resetGame() {
    snake = [
      { x: 7, y: 12 },
      { x: 6, y: 12 },
      { x: 5, y: 12 }
    ];
    direction = { x: 1, y: 0 };
    nextDirection = { x: 1, y: 0 };
    score = 0;
    speedMs = INITIAL_SPEED_MS;
    running = true;
    paused = false;
    gameOver = false;
    scoreSubmitted = false;
    lastFrameTime = 0;
    placeFood();
    updateScoreboard();
    setState('Playing');
    setMessage('');
    pauseButton.disabled = false;
    pauseButton.textContent = 'Pause';
  }

  function startGame() {
    var playerName = sanitizeName(nameInput.value);

    if (!playerName) {
      setMessage('Enter a name first.', true);
      nameInput.focus();
      return;
    }

    nameInput.value = playerName;
    resetGame();
    cancelAnimationFrame(animationFrameId);
    animationFrameId = requestAnimationFrame(gameLoop);
    canvas.focus();
  }

  function pauseGame() {
    if (!running || gameOver)
      return;

    paused = !paused;
    pauseButton.textContent = paused ? 'Resume' : 'Pause';
    setState(paused ? 'Paused' : 'Playing');

    if (!paused) {
      lastFrameTime = 0;
      animationFrameId = requestAnimationFrame(gameLoop);
    }
  }

  function endGame() {
    running = false;
    gameOver = true;
    pauseButton.disabled = true;
    setState('Done');
    saveLocalBestScore();
    drawBoard();
    submitScore();
  }

  function isSameCell(a, b) {
    return a.x === b.x && a.y === b.y;
  }

  function cellIsOnSnake(cell) {
    return snake.some(function(segment) {
      return isSameCell(segment, cell);
    });
  }

  function placeFood() {
    var openCells = [];

    for (var y = 0; y < GRID_SIZE; y += 1) {
      for (var x = 0; x < GRID_SIZE; x += 1) {
        var cell = { x: x, y: y };

        if (!cellIsOnSnake(cell))
          openCells.push(cell);
      }
    }

    food = openCells[Math.floor(Math.random() * openCells.length)] || { x: 0, y: 0 };
  }

  function changeDirection(newDirection) {
    if (!running || paused)
      return;

    if (newDirection.x + direction.x === 0 && newDirection.y + direction.y === 0)
      return;

    nextDirection = newDirection;
  }

  function stepGame() {
    direction = nextDirection;

    var head = snake[0];
    var nextHead = {
      x: head.x + direction.x,
      y: head.y + direction.y
    };
    var willEat = isSameCell(nextHead, food);
    var collisionBody = willEat ? snake : snake.slice(0, -1);

    if (
      nextHead.x < 0 ||
      nextHead.x >= GRID_SIZE ||
      nextHead.y < 0 ||
      nextHead.y >= GRID_SIZE ||
      collisionBody.some(function(segment) { return isSameCell(segment, nextHead); })
    ) {
      endGame();
      return;
    }

    snake.unshift(nextHead);

    if (willEat) {
      score += SCORE_STEP;
      speedMs = Math.max(MIN_SPEED_MS, INITIAL_SPEED_MS - Math.floor(score / 50) * 6);
      updateScoreboard();
      placeFood();
    } else {
      snake.pop();
    }
  }

  function drawRoundedCell(x, y, size, radius, fillStyle) {
    context.fillStyle = fillStyle;
    context.beginPath();
    context.roundRect(x, y, size, size, radius);
    context.fill();
  }

  function drawGrid(cellSize) {
    context.strokeStyle = 'rgba(255, 255, 255, 0.055)';
    context.lineWidth = 1;

    for (var index = 1; index < GRID_SIZE; index += 1) {
      var position = Math.floor(index * cellSize) + 0.5;
      context.beginPath();
      context.moveTo(position, 0);
      context.lineTo(position, canvas.height);
      context.stroke();
      context.beginPath();
      context.moveTo(0, position);
      context.lineTo(canvas.width, position);
      context.stroke();
    }
  }

  function drawBoard() {
    var cellSize = canvas.width / GRID_SIZE;
    var gap = Math.max(2, cellSize * 0.08);
    var tileSize = cellSize - gap * 2;
    var radius = Math.max(3, cellSize * 0.18);

    context.fillStyle = '#111820';
    context.fillRect(0, 0, canvas.width, canvas.height);
    drawGrid(cellSize);

    drawRoundedCell(
      food.x * cellSize + gap,
      food.y * cellSize + gap,
      tileSize,
      radius,
      '#f0b44c'
    );

    snake.forEach(function(segment, index) {
      drawRoundedCell(
        segment.x * cellSize + gap,
        segment.y * cellSize + gap,
        tileSize,
        radius,
        index === 0 ? '#7bd0c1' : '#56a898'
      );
    });

    if (!running && gameOver) {
      context.fillStyle = 'rgba(17, 24, 32, 0.72)';
      context.fillRect(0, 0, canvas.width, canvas.height);
      context.fillStyle = '#ffffff';
      context.font = '700 34px Arial, sans-serif';
      context.textAlign = 'center';
      context.fillText('Game Over', canvas.width / 2, canvas.height / 2 - 10);
      context.font = '600 20px Arial, sans-serif';
      context.fillText('Score: ' + score, canvas.width / 2, canvas.height / 2 + 28);
    }
  }

  function gameLoop(timestamp) {
    if (!running || paused)
      return;

    if (!lastFrameTime)
      lastFrameTime = timestamp;

    if (timestamp - lastFrameTime >= speedMs) {
      stepGame();
      drawBoard();
      lastFrameTime = timestamp;
    }

    if (running)
      animationFrameId = requestAnimationFrame(gameLoop);
  }

  function resizeCanvas() {
    var wrapper = canvas.parentElement;
    var displaySize = Math.min(wrapper.clientWidth || 576, 576);

    if (!displaySize)
      return;

    canvas.style.width = displaySize + 'px';
    canvas.style.height = displaySize + 'px';
    canvas.width = displaySize;
    canvas.height = displaySize;
    drawBoard();
  }

  function directionFromKey(key) {
    var normalized = String(key || '').toLowerCase();

    if (normalized === 'arrowup' || normalized === 'w')
      return { x: 0, y: -1 };
    if (normalized === 'arrowdown' || normalized === 's')
      return { x: 0, y: 1 };
    if (normalized === 'arrowleft' || normalized === 'a')
      return { x: -1, y: 0 };
    if (normalized === 'arrowright' || normalized === 'd')
      return { x: 1, y: 0 };

    return null;
  }

  function directionFromName(name) {
    if (name === 'up')
      return { x: 0, y: -1 };
    if (name === 'down')
      return { x: 0, y: 1 };
    if (name === 'left')
      return { x: -1, y: 0 };
    if (name === 'right')
      return { x: 1, y: 0 };

    return null;
  }

  function bindEvents() {
    startButton.addEventListener('click', startGame);
    pauseButton.addEventListener('click', pauseGame);
    refreshButton.addEventListener('click', loadLeaderboard);

    nameInput.addEventListener('input', function() {
      var cleanName = sanitizeName(nameInput.value);

      if (cleanName !== nameInput.value)
        nameInput.value = cleanName;
    });

    document.addEventListener('keydown', function(event) {
      var newDirection = directionFromKey(event.key);

      if (!newDirection)
        return;

      if (document.activeElement === nameInput && !running)
        return;

      event.preventDefault();
      changeDirection(newDirection);
    });

    document.querySelectorAll('[data-direction]').forEach(function(button) {
      button.addEventListener('click', function() {
        changeDirection(directionFromName(button.getAttribute('data-direction')));
      });
    });

    canvas.addEventListener('touchstart', function(event) {
      var touch = event.touches[0];
      touchStart = { x: touch.clientX, y: touch.clientY };
    }, { passive: true });

    canvas.addEventListener('touchmove', function(event) {
      if (!touchStart)
        return;

      var touch = event.touches[0];
      var diffX = touch.clientX - touchStart.x;
      var diffY = touch.clientY - touchStart.y;

      if (Math.max(Math.abs(diffX), Math.abs(diffY)) < 24)
        return;

      event.preventDefault();

      if (Math.abs(diffX) > Math.abs(diffY))
        changeDirection(diffX > 0 ? { x: 1, y: 0 } : { x: -1, y: 0 });
      else
        changeDirection(diffY > 0 ? { x: 0, y: 1 } : { x: 0, y: -1 });

      touchStart = null;
    }, { passive: false });

    window.addEventListener('resize', resizeCanvas);
  }

  document.addEventListener('DOMContentLoaded', function() {
    canvas = document.getElementById('snakeBoard');
    context = canvas ? canvas.getContext('2d') : null;
    nameInput = document.getElementById('snakePlayerName');
    startButton = document.getElementById('snakeStart');
    pauseButton = document.getElementById('snakePause');
    refreshButton = document.getElementById('snakeRefreshScores');
    scoreElement = document.getElementById('snakeScore');
    bestElement = document.getElementById('snakeBest');
    stateElement = document.getElementById('snakeState');
    messageElement = document.getElementById('snakeMessage');
    leaderboardElement = document.getElementById('snakeLeaderboard');

    if (!canvas || !context || !nameInput || !startButton || !pauseButton || !leaderboardElement)
      return;

    if (!context.roundRect) {
      context.roundRect = function(x, y, width, height, radius) {
        this.moveTo(x + radius, y);
        this.lineTo(x + width - radius, y);
        this.quadraticCurveTo(x + width, y, x + width, y + radius);
        this.lineTo(x + width, y + height - radius);
        this.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
        this.lineTo(x + radius, y + height);
        this.quadraticCurveTo(x, y + height, x, y + height - radius);
        this.lineTo(x, y + radius);
        this.quadraticCurveTo(x, y, x + radius, y);
      };
    }

    db = initializeFirebase();
    loadLocalBestScore();
    resetGame();
    running = false;
    pauseButton.disabled = true;
    setState('Ready');
    bindEvents();
    resizeCanvas();
    loadLeaderboard();
  });
})();
