export { Autocomplete };

class Autocomplete {
    #element_id;
    #search_form_id;
    #try_add_player_name;
    #index;

    constructor(element_id, search_form_id, add_player_cb) {
        this.#element_id = element_id;
        this.#search_form_id = search_form_id;
        this.#try_add_player_name = add_player_cb;
        this.#index = -1;

        this.init();
    }

    init() {
        let result = document.getElementById(this.#element_id);
        result.classList.add('hidden');
    }

    populate(results) {
        let element = document.getElementById(this.#element_id);
        let list = document.createElement('ul');
        for (const result of results) {
            let entry = document.createElement('li');
            entry.innerText = result;
            list.appendChild(entry);
        }

        element.replaceChildren(list);
        element.classList.remove('hidden');
        this.#index = -1;
    }

    keyevents(ev) {
        let search_form = document.getElementById(this.#search_form_id);
        let result = document.getElementById(this.#element_id);
        // Sanity check there is a list and if not, reset to nothing.
        const list = result.firstChild;
        if (!list) {
            this.#index = -1;
            return;
        }

        if (ev.code === "ArrowUp" || ev.code === "ArrowDown") {
            ev.preventDefault();

            let offset = 0;
            if (ev.code === "ArrowUp") {
                offset = -1;
            }
            else {
                offset = 1;
            }
            this.#index = Math.max(0, Math.min(this.#index + offset, list.childNodes.length - 1));

            for (let i = 0; i < list.childNodes.length; i++) {
                let entry = list.childNodes[i];
                if (i === this.#index) {
                    entry.classList.add("hovered");
                    entry.scrollIntoView({block: "nearest", inline: "nearest"});
                }
                else {
                    entry.classList.remove("hovered");
                }
            }
        }
        else if (ev.code === "Enter") {
            if (this.#index >= 0 && this.#index < (list.childNodes.length - 1)) {
                let entry = list.childNodes[this.#index];
                const player_name = entry.innerText;
                if (this.#try_add_player_name(player_name)) {
                    search_form.value = "";
                    this.hide();
                }
            }
        }
    }

    click(ev) {
        const player_name = ev.target.innerText;
        if (this.#try_add_player_name(player_name)) {
            let search_form = document.getElementById(this.#search_form_id);
            search_form.value = "";
            this.hide();
        }
    }

    hide() {
        let result = document.getElementById(this.#element_id);
        result.classList.add('hidden');
        this.#index = -1;
    }
}
