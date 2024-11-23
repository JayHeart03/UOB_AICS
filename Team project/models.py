from django.db import models
from django import forms
from django.core.validators import MaxValueValidator, MinValueValidator


class User(models.Model):
    user_id = models.AutoField(primary_key=True, default=1)
    username = models.CharField(max_length=50)
    first_name = models.CharField(max_length=50)
    last_name = models.CharField(max_length=50)

    SPORT_CHOICES = (
        ('cricket', 'Cricket'),
        ('football', 'Football'),
        ('volleyball', 'Volleyball'),
        ('field hockey', 'Field Hockey'),
        ('tennis', 'Tennis'),
        ('basketball', 'Basketball'),
        ('table tennis', 'Table Tennis'),
        ('baseball', 'Baseball'),
        ('track and field', 'Track and Field'),
        ('golf', 'Golf'),
        ('rugby', 'Rugby'),
        ('badminton', 'Badminton'),
        ('american football', 'American Football'),
        ('swimming', 'Swimming'),
        ('gymnastics', 'Gymnastics'),
        ('cycling', 'Cycling'),
        ('ice hockey', 'Ice Hockey'),
        ('handball', 'Handball'),
        ('rock climbing', 'Rock Climbing'),
        ('frisbee', 'Frisbee'),
        ('bowling', 'Bowling'),
    )
    sport_of_choice = models.CharField(
        max_length=50, choices=SPORT_CHOICES, default='other')

    SKILL_CHOICES = [
        ('beginner', 'Beginner'),
        ('intermediate', 'Intermediate'),
        ('advanced', 'Advanced'),
    ]
    skill_level = models.CharField(
        max_length=50, choices=SKILL_CHOICES, default='beginner')

    PLAY_FREQUENCY_CHOICES = [
        ('daily', 'Daily'),
        ('weekly', 'Weekly'),
        ('monthly', 'Monthly'),
        ('yearly', 'Yearly'),
    ]
    play_frequency = models.CharField(
        max_length=50, choices=PLAY_FREQUENCY_CHOICES, default='monthly')

    MOTIVATION_CHOICES = [
        ('fun', 'Fun'),
        ('competition', 'Competition'),
        ('health', 'Health'),
        ('social', 'Social'),
    ]
    motivation = models.CharField(
        max_length=50, choices=MOTIVATION_CHOICES, default='fun')

    PLAYING_ENVIRONMENT_CHOICES = [
        ('indoor', 'Indoor'),
        ('outdoor', 'Outdoor'),
        ('both', 'Both'),
    ]
    playing_environment = models.CharField(
        max_length=50, choices=PLAYING_ENVIRONMENT_CHOICES, default='both')

    DAYS_CHOICES = [
        ('monday', 'Monday'),
        ('tuesday', 'Tuesday'),
        ('wednesday', 'Wednesday'),
        ('thursday', 'Thursday'),
        ('friday', 'Friday'),
        ('saturday', 'Saturday'),
        ('sunday', 'Sunday'),
    ]
    days_available = models.CharField(
        max_length=50, choices=DAYS_CHOICES, default='monday')

    location = models.CharField(max_length=200)
    location_preference = models.CharField(max_length=200)

    GENDER_CHOICES = [
        ('male', 'Male'),
        ('female', 'Female'),
        ('other', 'Other'),
    ]
    gender = models.CharField(
        max_length=10, choices=GENDER_CHOICES, default='other')

    date_of_birth = models.DateField()
    residence = models.CharField(max_length=50)
    email = models.EmailField(max_length=254)
    number = models.CharField(max_length=20)
    date_created = models.DateField()

    def __str__(self):
        return self.username


class Game(models.Model):
    game_id = models.AutoField(primary_key=True, default=0)
    user_id = models.ForeignKey(User, on_delete=models.CASCADE, default=1)
    session_title = models.CharField(max_length=50, default='')
    sport_of_choice = (
        ('cricket', 'Cricket'),
        ('football', 'Football'),
        ('volleyball', 'Volleyball'),
        ('field hockey', 'Field Hockey'),
        ('tennis', 'Tennis'),
        ('basketball', 'Basketball'),
        ('table tennis', 'Table Tennis'),
        ('baseball', 'Baseball'),
        ('track and field', 'Track and Field'),
        ('golf', 'Golf'),
        ('rugby', 'Rugby'),
        ('badminton', 'Badminton'),
        ('american football', 'American Football'),
        ('swimming', 'Swimming'),
        ('gymnastics', 'Gymnastics'),
        ('cycling', 'Cycling'),
        ('ice hockey', 'Ice Hockey'),
        ('handball', 'Handball'),
        ('rock climbing', 'Rock Climbing'),
        ('frisbee', 'Frisbee'),
        ('bowling', 'Bowling'),
    )
    confirmed_players = models.PositiveIntegerField(default=0)
    skill_rating = models.CharField(
        max_length=50,
        choices=[
            ('Beginner', 'Beginner'),
            ('Intermediate', 'Intermediate'),
            ('Advanced', 'Advanced')
        ],
        default='Beginner'
    )
    age_range = models.CharField(
        max_length=50,
        choices=[
            ('18-25', '18-25'),
            ('26-35', '26-35'),
            ('36-45', '36-45'),
            ('46-55', '46-55'),
            ('56+', '56+')
        ],
        default='18-25'
    )
    game_description = models.TextField(default='')
    time = models.TimeField(default='00:00:00')
    date = models.DateField(default='2000-01-01')
    location = models.CharField(max_length=200, default='')
    frequency = models.CharField(
        max_length=50,
        choices=[
            ('Daily', 'Daily'),
            ('Weekly', 'Weekly'),
            ('Monthly', 'Monthly')
        ],
        default='Weekly'
    )

    def __str__(self):
        return self.session_title


class Player(models.Model):
    game = models.ForeignKey(Game, on_delete=models.CASCADE)
    user = models.ForeignKey(User, on_delete=models.CASCADE)

    def __str__(self):
        return f"{self.user.username} ({self.game.session_title})"


class Friend(models.Model):
    user_1 = models.ForeignKey(
        User, on_delete=models.CASCADE, related_name='friends_user_1')
    user_2 = models.ForeignKey(
        User, on_delete=models.CASCADE, related_name='friends_user_2')
    status = models.CharField(max_length=10, choices=[(
        'Pending', 'Pending'), ('Accepted', 'Accepted'), ('Rejected', 'Rejected')])

    def __str__(self):
        return f'{self.user_1.username} and {self.user_2.username}'


class FriendRequest(models.Model):
    from_user = models.ForeignKey(
        User, on_delete=models.CASCADE, related_name='friend_requests_sent')
    to_user = models.ForeignKey(
        User, on_delete=models.CASCADE, related_name='friend_requests_received')
    status = models.CharField(max_length=10, choices=[(
        'Pending', 'Pending'), ('Accepted', 'Accepted'), ('Rejected', 'Rejected')])

    def __str__(self):
        return f'{self.from_user.username} sent friend request to {self.to_user.username}'
