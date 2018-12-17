def go_outside(int dow, string weather) int
{
      if (dow == 5 or dow == 6 and strcmp(weather, 'horrible') != 0)
        return 1;
      return 0;
}

print(go_outside(5, 'great'));
