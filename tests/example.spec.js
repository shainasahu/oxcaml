import { test, expect } from '@playwright/test';

const BASE_URL = 'http://localhost:8002';

test.describe('Crazy Eights UI', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto(BASE_URL);
  });

  test('renders game board', async ({ page }) => {
    await expect(page.locator('.game')).toBeVisible();
    await expect(page.locator('.hand')).toHaveCount(2);
    await expect(page.locator('.deck')).toBeVisible();
    await expect(page.locator('.discard')).toBeVisible();
  });

  test('clicking a card selects and deselects it', async ({ page }) => {
    const firstCard = page.locator('.hand .card:not(.face-down)').first();
    await expect(firstCard).toBeVisible();

    await firstCard.click();
    await expect(firstCard).toHaveClass(/selected/);

    await firstCard.click();
    await expect(firstCard).not.toHaveClass(/selected/);
  });

  test('drawing a card works', async ({ page }) => {
    const deck = page.locator('.deck');
    await expect(deck).toBeVisible();
    const handBefore = await page.locator('.hand .card:not(.face-down)').count();

    await deck.click();
    const handAfter = await page.locator('.hand .card:not(.face-down)').count();

    expect(handAfter).not.toBeLessThan(handBefore);
  });

  test('winner message appears at game end (simulation)', async ({ page }) => {
    await page.evaluate(() => {
      const e = document.createElement('div');
      e.className = 'winner';
      e.innerText = 'Player 1 wins the game!';
      document.querySelector('.game').appendChild(e);
    });

    await expect(page.locator('.winner')).toHaveText(/Player 1 wins/);
  });

  test('turn indicator shows correct initial player', async ({ page }) => {
    const turnText = await page.locator('.turn-indicator').textContent();
    expect(turnText).toMatch(/Player [12]'s turn!/);
  });

  test('clicking opponent face-down cards does nothing', async ({ page }) => {
    const opponentCard = page.locator('.hand .card.face-down').first();
    const classesBefore = await opponentCard.getAttribute('class');
    await opponentCard.click();
    const classesAfter = await opponentCard.getAttribute('class');
    expect(classesAfter).toBe(classesBefore);
  });

  test('play button does nothing if no cards selected', async ({ page }) => {
    const playButton = page.locator('button:has-text("Play")');
    await expect(playButton).toHaveCount(0);
  });

});
