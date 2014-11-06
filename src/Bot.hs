module Bot (bot) where

import Vindinium.Types
import StupidBot.Bot (stupidBot)
import DumbBot.Bot (dumbBot)

bot :: Bot
--bot = stupidBot
bot = dumbBot
