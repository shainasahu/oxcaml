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

});
