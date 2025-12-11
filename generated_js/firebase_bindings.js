// generated_js/firebase_bindings.js
import { getFirestore, collection, doc, setDoc, getDoc, onSnapshot } from "https://www.gstatic.com/firebasejs/12.6.0/firebase-firestore.js";

// Firestore instance (initialized via index.html)
const db = getFirestore();

// Simple utility to generate random IDs
function randomId() {
    return Math.random().toString(36).substring(2, 10);
}

// Room / Player functions:

// Create a new room
export async function createRoom() {
    const roomId = randomId();
    const roomRef = doc(collection(db, "rooms"), roomId);

    // Initial room document
    await setDoc(roomRef, {
        players: {},
        gameState: {},   // placeholder; OCaml will update later
        createdAt: Date.now()
    });

    return roomId;
}

// Join an existing room
export async function joinRoom(roomId, playerId) {
    const roomRef = doc(db, "rooms", roomId);
    const roomSnap = await getDoc(roomRef);
    if (!roomSnap.exists()) {
        throw new Error("Room does not exist");
    }

    const roomData = roomSnap.data();
    roomData.players[playerId] = { joinedAt: Date.now() };

    await setDoc(roomRef, roomData);
    return true;
}

// Listen for updates to a room
export function listenRoom(roomId, callback) {
    const roomRef = doc(db, "rooms", roomId);
    return onSnapshot(roomRef, (snapshot) => {
        if (snapshot.exists()) {
            callback(snapshot.data());
        }
    });
}

// Send a move or update to Firestore
export async function sendMove(roomId, playerId, moveData) {
    const roomRef = doc(db, "rooms", roomId);
    const roomSnap = await getDoc(roomRef);
    if (!roomSnap.exists()) throw new Error("Room not found");

    const roomData = roomSnap.data();
    roomData.players[playerId].lastMove = moveData;
    await setDoc(roomRef, roomData);
}
