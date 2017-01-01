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
<div id="customForms">
    <input class="form-control" type="email" placeholder="Email" style="margin-top: 20px; max-width: 400px;" id="firstField">
    <input class="form-control" type="email" placeholder="Confirm Email" style="margin-bottom: 10.5px; max-width: 400px;" id="secondField">
    <button id="customSubmitButton" class="btn btn-primary">Submit</button>
</div>
