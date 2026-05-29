(function() {
  var OWNER_EMAIL = 'smulherin519@gmail.com';
  var MAX_FILE_SIZE = 20 * 1024 * 1024;
  var ALLOWED_TYPES = [
    /^image\//,
    /^text\//,
    /^video\/mp4$/,
    /^application\/pdf$/,
    /^application\/msword$/,
    /^application\/vnd\.openxmlformats-officedocument\./,
    /^application\/vnd\.ms-/,
    /^text\/csv$/
  ];

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

  function textWithBreaks(element, text) {
    String(text || '').split('\n').forEach(function(line, index) {
      if (index)
        element.appendChild(document.createElement('br'));

      element.appendChild(document.createTextNode(line));
    });
  }

  function formatDate(timestamp) {
    if (!timestamp || typeof timestamp.toDate !== 'function')
      return 'Just now';

    return timestamp.toDate().toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
      hour: 'numeric',
      minute: '2-digit'
    });
  }

  function sanitizeFileName(name) {
    return String(name || 'attachment')
      .replace(/[/\\?%*:|"<>]/g, '-')
      .replace(/\s+/g, '-')
      .replace(/-+/g, '-')
      .slice(0, 120);
  }

  function isAllowedFile(file) {
    if (file.size > MAX_FILE_SIZE)
      return false;

    return ALLOWED_TYPES.some(function(pattern) {
      return pattern.test(file.type || 'application/octet-stream');
    });
  }

  function isOwner(user) {
    return !!user
      && String(user.email || '').toLowerCase() === OWNER_EMAIL
      && user.emailVerified;
  }

  function showAdminPanel() {
    var params = new URLSearchParams(window.location.search);
    return params.get('admin') === '1' || window.location.hash === '#admin';
  }

  document.addEventListener('DOMContentLoaded', function() {
    if (!window.firebase)
      return;

    if (!firebase.apps.length)
      firebase.initializeApp(firebaseConfig);

    var auth = firebase.auth();
    var db = firebase.firestore();
    var storage = firebase.storage();
    var provider = new firebase.auth.GoogleAuthProvider();

    var postsContainer = document.getElementById('newPostsContainer');
    var adminPanel = document.getElementById('blogAdmin');
    var editor = document.getElementById('blogEditor');
    var signInBtn = document.getElementById('blogSignIn');
    var signOutBtn = document.getElementById('blogSignOut');
    var authStatus = document.getElementById('blogAuthStatus');
    var postStatus = document.getElementById('blogPostStatus');
    var submitBtn = document.getElementById('submitBtn');
    var titleInput = document.getElementById('blogTitleInput');
    var postInput = document.getElementById('blogPostInput');
    var fileInput = document.getElementById('blogAttachmentInput');

    if (!postsContainer)
      return;

    if (adminPanel)
      adminPanel.hidden = !showAdminPanel();

    function setStatus(message) {
      if (postStatus)
        postStatus.textContent = message || '';
    }

    function renderAttachments(post, postDiv) {
      var attachments = Array.isArray(post.attachments) ? post.attachments : [];

      if (!attachments.length)
        return;

      var list = document.createElement('ul');
      list.className = 'post-attachments';

      attachments.forEach(function(attachment) {
        if (!attachment || !attachment.url)
          return;

        var item = document.createElement('li');
        var link = document.createElement('a');
        link.href = attachment.url;
        link.target = '_blank';
        link.rel = 'noopener';
        link.textContent = attachment.name || 'Attachment';
        item.appendChild(link);

        if (/^image\//.test(attachment.contentType || '')) {
          var image = document.createElement('img');
          image.src = attachment.url;
          image.alt = attachment.name || '';
          item.appendChild(image);
        }

        list.appendChild(item);
      });

      postDiv.appendChild(list);
    }

    async function loadPosts() {
      postsContainer.textContent = '';

      try {
        var querySnapshot = await db.collection('posts').orderBy('createdAt', 'desc').get();

        querySnapshot.forEach(function(doc) {
          var post = doc.data();
          var postDiv = document.createElement('article');
          var titleHeader = document.createElement('h1');
          var dateHeader = document.createElement('h4');
          var postContent = document.createElement('p');

          postDiv.className = 'blog-post';
          titleHeader.textContent = post.title || 'Untitled';
          dateHeader.textContent = formatDate(post.createdAt);
          postContent.className = 'blog-post-content';
          textWithBreaks(postContent, post.content);

          postDiv.appendChild(titleHeader);
          postDiv.appendChild(dateHeader);
          postDiv.appendChild(postContent);
          renderAttachments(post, postDiv);
          postsContainer.appendChild(postDiv);
        });
      } catch (error) {
        console.error('Error loading posts:', error);
      }
    }

    async function uploadAttachments(postId, files) {
      var attachments = [];

      for (var index = 0; index < files.length; index += 1) {
        var file = files[index];

        if (!isAllowedFile(file))
          throw new Error(file.name + ' is not an allowed attachment type or is larger than 20 MB.');

        var storageName = Date.now() + '-' + index + '-' + sanitizeFileName(file.name);
        var path = 'blog/' + postId + '/' + storageName;
        var ref = storage.ref(path);
        var snapshot = await ref.put(file, {
          contentType: file.type || 'application/octet-stream',
          customMetadata: { postId: postId }
        });
        var url = await snapshot.ref.getDownloadURL();

        attachments.push({
          name: file.name,
          path: path,
          url: url,
          contentType: file.type || 'application/octet-stream',
          size: file.size
        });
      }

      return attachments;
    }

    async function publishPost() {
      var user = auth.currentUser;
      var titleText = titleInput.value.trim();
      var postText = postInput.value.trim();

      if (!isOwner(user)) {
        setStatus('Sign in with the portfolio owner account first.');
        return;
      }

      if (!titleText || !postText) {
        setStatus('Title and post content cannot be empty.');
        return;
      }

      submitBtn.disabled = true;
      setStatus('Publishing...');

      try {
        var docRef = db.collection('posts').doc();
        var files = Array.prototype.slice.call(fileInput.files || []);
        var attachments = await uploadAttachments(docRef.id, files);

        await docRef.set({
          title: titleText,
          content: postText,
          attachments: attachments,
          authorEmail: user.email,
          createdAt: firebase.firestore.FieldValue.serverTimestamp(),
          updatedAt: firebase.firestore.FieldValue.serverTimestamp()
        });

        titleInput.value = '';
        postInput.value = '';
        fileInput.value = '';
        setStatus('Published.');
        await loadPosts();
      } catch (error) {
        console.error('Error publishing post:', error);
        setStatus(error.message || 'There was an error publishing this post.');
      } finally {
        submitBtn.disabled = false;
      }
    }

    if (signInBtn) {
      signInBtn.addEventListener('click', function() {
        auth.signInWithPopup(provider).catch(function(error) {
          console.error('Sign-in error:', error);
          if (authStatus)
            authStatus.textContent = 'Sign-in failed.';
        });
      });
    }

    if (signOutBtn) {
      signOutBtn.addEventListener('click', function() {
        auth.signOut();
      });
    }

    if (submitBtn)
      submitBtn.addEventListener('click', publishPost);

    auth.onAuthStateChanged(function(user) {
      var owner = isOwner(user);

      if (authStatus) {
        authStatus.textContent = user
          ? (owner ? 'Signed in as ' + user.email : user.email + ' is not authorized to publish.')
          : 'Sign in with Google to publish.';
      }

      if (editor)
        editor.hidden = !owner;

      if (signInBtn)
        signInBtn.hidden = !!user;

      if (signOutBtn)
        signOutBtn.hidden = !user;
    });

    loadPosts();
  });
})();
