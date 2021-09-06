import random
list_of_cards = ["Ace", 2, 3, 4, 5, 6, 7, 8, 9, 10, "Jack", "Queen", "King"]


class Carddeck(object):
    def __init__(self):
        deck = []
        self.deck = deck

    def add_to_deck(self, suite):
        for a in list_of_cards:
            self.deck.append(f"{a} of {suite}")


cards = Carddeck()
cards.add_to_deck("Spades")
cards.add_to_deck("Hearts")
cards.add_to_deck("Clubs")
cards.add_to_deck("Diamonds")
random.shuffle(cards.deck)
cardnumber = 0


class Person(Carddeck):
    def __init__(self, name):
        super().__init__()
        self.cards = []
        self.name = name
        self.score = 0
        self.deck = cards.deck
        self.bet = 0
        self.money = 0

    def get_current_score(self):
        return self.score

    def reset_round(self):
        self.cards = []
        self.score = 0

    def set_bet(self, multiplier):
        self.bet *= multiplier

    def set_money(self, operator, value):
        if operator == "-":
            self.money -= value
        if operator == "+":
            self.money += value

    def get_bet(self):
        return self.bet

    def get_money(self):
        return self.money

    def get_name(self):
        return self.name

    def get_cards(self):
        return self.cards

    def add_card(self):
        global cardnumber
        chosen_card_beforesplit = self.deck[cardnumber]
        chosen_card_beforesplit = chosen_card_beforesplit.split(" ")
        chosen_card = chosen_card_beforesplit[0]
        if chosen_card.isdigit():
            chosen_card = int(chosen_card)
        self.cards.append(chosen_card)
        cardnumber += 1
        if chosen_card == 2 or chosen_card == 3 or chosen_card == 4 or chosen_card == 5 or chosen_card == 6 or \
                chosen_card == 7 or chosen_card == 8 or chosen_card == 9 or chosen_card == 10:
            self.score += int(chosen_card)
        elif chosen_card == "Jack" or chosen_card == "Queen" or chosen_card == "King":
            self.score += 10
        if self.name == "dealer":
            if chosen_card == "Ace":
                if self.score < 11:
                    self.score += 11
                else:
                    self.score += 1
        else:
            if chosen_card == "Ace":
                ace_score = int(input("Do you want the Ace to be 1 or 11: "))
                self.score += ace_score

        return chosen_card


player_count = int(input("How many players: "))
players = []
for i in range(player_count):
    player_name = str(input("What is your name: "))
    players.append(Person(player_name))
    players[i].bet = int(input(f"What is your starting bet {player_name}: "))
dealer = Person("dealer")


while True:
    for player in players:
        player.reset_round()
        dealer.reset_round()

        for i in range(2):
            player.add_card()
            dealer.add_card()

        hit_choice = input(f"{player.name}, your cards are {player.get_cards()[0]} and {player.get_cards()[1]}\n\n"
                           f"The dealer's cards are {dealer.get_cards()[0]} and ***\n"
                           f"Would you like to hit: ")
        hit_check_score = player.get_current_score()

        while hit_choice == "yes" and hit_check_score < 21:
            hit_added_card = player.add_card()
            hit_check_score = player.get_current_score()

            if hit_added_card == "Ace":
                print(f"{player.get_name()}, you drew an {hit_added_card}")
            else:
                print(f"{player.get_name()}, you drew a {hit_added_card}")
            print(f"{player.get_name()}, your current score is {hit_check_score}")

            if hit_check_score < 21:
                hit_choice = input(f"Do you want to hit again {player.get_name()}: ")

        print(f"{player.get_name()}, your score for this round is {player.get_current_score()}")

    print(f"The dealer's cards are {dealer.get_cards()[0]} and {dealer.get_cards()[1]}")

    while dealer.get_current_score() < 17:
        dealer_added_card = dealer.add_card()

        if dealer_added_card == "Ace":
            print(f"The dealer drew an {dealer_added_card}")
        else:
            print(f"The dealer drew a {dealer_added_card}")

    print(f"The dealer's score is {dealer.get_current_score()}")

    for player in players:
        if player.get_current_score() == 21 and dealer.get_current_score() != 21:
            print(f"{player.get_name()} you got a blackjack!!!")
            player.set_bet(2.5)
        elif dealer.get_current_score() > 21 and player.get_current_score() <= 21:
            print(f"{player.get_name()} you won!!!")
            player.set_bet(2)
        elif player.get_current_score() > 21:
            print(f"{player.get_name()}, the dealer won")
            player.set_bet(0)
        elif dealer.get_current_score() <= 21 and player.get_current_score() <= 21:
            if dealer.get_current_score() == player.get_current_score():
                print(f"{player.get_name()}, this round is a tie")
            elif dealer.get_current_score() > player.get_current_score():
                print(f"{player.get_name()}, the dealer won")
                player.set_bet(0)
            elif dealer.get_current_score() < player.get_current_score():
                print(f"{player.get_name()}, you won!!!")
                player.set_bet(2)
        player.set_money("+", player.get_bet())

    for player in players:
        play_another_round = input(f"Would you like to play another round or cash out {player.get_name()}: "
                                   f"You have {player.get_money()} dollars: ")
        if play_another_round != "yes":
            players.remove(player)
            continue

        player.bet = int(input(f"How much would you like to bet this round {player.get_name()}: "))
        player.set_money("-", player.get_bet())

        while player.get_money() < 0:
            reduce_or_add = input(f"You need to either reduce your bet or add more money in {player.get_name()}.\n"
                                  "Please type reduce or add: ")
            if reduce_or_add == "reduce":
                new_bet = int(input(f"What would you like your new bet to be {player.get_name()}?"))
                player.set_money("+", player.get_bet() - new_bet)
                if player.get_money() >= 1:
                    player.bet = new_bet
            if reduce_or_add == "add":
                money_added = int(input(f"How much money would you like to add {player.get_name()}: "))
                player.set_money("+", money_added)
