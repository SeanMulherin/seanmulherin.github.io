import { getVercelOidcToken } from '@vercel/oidc';

const origin = 'https://seanmulherin.github.io';
const database = 'projects/portfolio-blog-f49c1/databases/(default)';
const audience = '//iam.googleapis.com/projects/884520998565/locations/global/workloadIdentityPools/portfolio-analytics/providers/vercel';
const pageviewId = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
let cachedToken;

async function databaseToken() {
  if (cachedToken && cachedToken.expiresAt > Date.now() + 60_000) return cachedToken.value;
  const response = await fetch('https://sts.googleapis.com/v1/token', {
    method: 'POST',
    body: new URLSearchParams({
      audience,
      grant_type: 'urn:ietf:params:oauth:grant-type:token-exchange',
      requested_token_type: 'urn:ietf:params:oauth:token-type:access_token',
      subject_token_type: 'urn:ietf:params:oauth:token-type:jwt',
      subject_token: await getVercelOidcToken(),
      scope: 'https://www.googleapis.com/auth/datastore'
    })
  });
  if (!response.ok) throw new Error('Token exchange HTTP ' + response.status);
  const { access_token, expires_in } = await response.json();
  cachedToken = { value: access_token, expiresAt: Date.now() + expires_in * 1000 };
  return access_token;
}

function coordinate(value, limit) {
  if (typeof value !== 'string' || !value.trim()) return null;
  const number = Number(value);
  if (!Number.isFinite(number) || Math.abs(number) > limit) return null;
  return Math.round(number * 10) / 10;
}

function locationFields(headers) {
  const lat = coordinate(headers['x-vercel-ip-latitude'], 90);
  const lon = coordinate(headers['x-vercel-ip-longitude'], 180);
  const country = headers['x-vercel-ip-country'];
  const region = headers['x-vercel-ip-country-region'];
  const known = lat !== null && lon !== null;
  return {
    country: /^[A-Z]{2}$/.test(country ?? '') ? { stringValue: country } : { nullValue: null },
    region: /^[A-Z0-9-]{1,12}$/.test(region ?? '') ? { stringValue: region } : { nullValue: null },
    latitude: known ? { doubleValue: lat } : { nullValue: null },
    longitude: known ? { doubleValue: lon } : { nullValue: null }
  };
}

export default async function handler(req, res) {
  res.setHeader('Cache-Control', 'no-store');
  if (req.headers.origin !== origin) return res.status(403).end();
  res.setHeader('Access-Control-Allow-Origin', origin);
  if (req.method !== 'POST') return res.status(405).setHeader('Allow', 'POST').end();
  if (req.headers.dnt === '1' || req.headers['sec-gpc'] === '1') return res.status(204).end();
  if (!/^text\/plain(?:;|$)/i.test(req.headers['content-type'] ?? '') ||
      typeof req.body !== 'string' || req.body.length !== 36 || !pageviewId.test(req.body)) {
    return res.status(400).end();
  }

  const fields = {
    visited_at: { timestampValue: new Date().toISOString() },
    path: { stringValue: '/' },
    ...locationFields(req.headers)
  };
  try {
    const response = await fetch(
      `https://firestore.googleapis.com/v1/${database}/documents/homepage_pageviews?documentId=${req.body}`,
      {
        method: 'POST',
        headers: { Authorization: `Bearer ${await databaseToken()}`, 'Content-Type': 'application/json' },
        body: JSON.stringify({ fields })
      }
    );
    if (response.ok || response.status === 409) return res.status(204).end();
    if (response.status === 401) cachedToken = undefined;
    console.error('Pageview write HTTP', response.status);
  } catch (error) {
    console.error('Pageview collection failed', error instanceof Error ? error.name : 'error');
  }
  return res.status(503).end();
}
