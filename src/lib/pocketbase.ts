import PocketBase from 'pocketbase';
import { env } from '$env/dynamic/public';

export const pb = new PocketBase(env.POCKETBASE_URL || 'https://data.suomenpalikkayhteiso.fi');
