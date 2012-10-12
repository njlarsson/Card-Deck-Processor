function deck(name) {
    return {
        cards: [],
        
        toString: function() {
            return name + "(" + this.cards.splice(0).reverse().join(", ") + ")";
        },

        read: function() {
            var cardsStr = prompt(name).split(/\s*,\s*/);
            var i = 0;
            while (i < cardsStr.length) {
                this.cards.push(parseInt(cardsStr[i]));
                i = i + 1;
            }
        },

        moveTopTo: function(other) {
            this.assertNotEmpty();
            other.cards.push(this.cards.pop());
        },

        moveAllTo: function(other) {
            this.cards.reverse();
            while (this.cards.length > 0) {
                other.cards.push(this.cards.pop());
            }
        },
        
        compareTop: function(other) {
            this.assertNotEmpty();
            other.assertNotEmpty();
            return this.cards[this.cards.length - 1] - other.cards[other.cards.length - 1];
        },

        isEmpty: function() {
            return this.cards.length == 0;
        },

        assertNotEmpty: function() {
            if (this.isEmpty()) {
                throw name + " is empty";
            }
        }
    }
}
