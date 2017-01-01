<script type="text/javascript">
    var customEmailForm = {
        $firstField: $("#firstField"),
        $secondField: $("#secondField"),
        $messageBox: $("#messageBox"),
        $submitButton: $("#customSubmitButton"),
        init: function(submitCallback) {
            this.submitCallback = submitCallback;
            this.$submitButton.click(this, this.onClick.bind(this));         
            this.$firstField.keypress(this, this.keypress.bind(this));         
            this.$secondField.keypress(this, this.keypress.bind(this));         
        },
        render: function() {
            this.$emailLoc.html(this.email);
        },
        onClick: function() {
            if(this.$firstField.val() != this.$secondField.val()) {
                this.displayMessage("Email Mismatch", "The two emails that you entered do not match.", false);
            } else if(this.$firstField.val() == "" || this.$secondField.val() == "") {
                this.displayMessage("Empty Fields", "Please fill your email twice in the boxes below.", false);
            } else {
                this.displayMessage("Success", "We've sent a verification email to "+this.$firstField.val()+".", true);
                this.submitCallback(this.$firstField.val());
            }
        },
        keypress: function(e) {
            if(e.which == 13) this.onClick();
        },
        displayMessage: function(title, message, isSuccess) {
            this.$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' id='messageCloseButton' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>"+title+"</strong>&nbsp;&nbsp;"+message+"</div>"))
        }
    }
</script>
<div id="githubForms">
    <button id="githubEmailButton" class="btn btn-primary">Use <span id="githubEmail"></span></button>
</div>
