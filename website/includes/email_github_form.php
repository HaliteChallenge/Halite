<script type="text/javascript">
    var githubSubmitButton = {
        $emailField: $("#githubEmail"),
        $button: $("#githubSubmitButton"),
        init: function(email, onSubmit) {
            this.onSubmit = onSubmit;

            this.$emailField.html(email);
            this.$button.click(this, this.onPress.bind(this));         
        },
        onPress: function() {
            this.displayMessage("Success", "You're all set!.", true);
            this.onSubmit();
        }
    }
</script>
<button id="githubEmailButton" class="btn btn-primary">Use <span id="githubEmail"></span></button>
