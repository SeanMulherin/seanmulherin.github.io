(() => {
    'use strict';

    if (location.origin !== 'https://seanmulherin.github.io' ||
        !['/', '/index.html'].includes(location.pathname)) return;

    function recordPageview() {
        const trackingPreferences = [navigator.doNotTrack, window.doNotTrack, navigator.msDoNotTrack];
        if (navigator.globalPrivacyControl || trackingPreferences.some(value => value === '1' || value === 'yes')) return;
        if (document.portfolioPageviewSent || !window.crypto?.randomUUID || !window.fetch) return;
        document.portfolioPageviewSent = true;

        fetch('https://portfolio-homepage-visits.vercel.app/api/pageview', {
            method: 'POST',
            body: crypto.randomUUID(),
            credentials: 'omit',
            referrerPolicy: 'no-referrer',
            keepalive: true
        }).catch(() => {});
    }

    if (document.prerendering) {
        document.addEventListener('prerenderingchange', recordPageview, { once: true });
    } else {
        recordPageview();
    }
})();
