class AvhWysiwygEditorToolkitTextElement extends HTMLElement {
  constructor() {
    super();

    var shadow = this.attachShadow({mode: 'closed'});

    this.mainDiv = document.createElement('div');
    this.mainDiv.style.display = 'inline-block';
    this.mainDiv.setAttribute('contenteditable', 'true');
    this.mainDiv.innerText = this.getAttribute('content');

    this.mainDiv.addEventListener("input", event => {
      var changedEvent = new CustomEvent(
        'content-changed',
        { detail: {'textContent': event.target.textContent}}
      );
      this.dispatchEvent(changedEvent);
    });

    shadow.appendChild(this.mainDiv);
  }

  static get observedAttributes() {return ['content']; }

  attributeChangedCallback(attr, oldValue, newValue) {
    if (attr == 'content') {
      if (this.mainDiv.textContent == newValue) {
        // Don't bother changing, since it will just mess up the cursor
      } else {
        this.mainDiv.innerHTML = newValue;
        // TODO: maybe retain cursor position? what would make sense?
      }
    }
  }
}

window.customElements.define('avh4-wysiwyg-editor-toolkit-text', AvhWysiwygEditorToolkitTextElement);

