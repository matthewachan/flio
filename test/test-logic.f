def go_outside(int dow, string weather) int
{
      if (dow == 5 or dow == 6 and weather != 'horrible')
        return 1;
      return 0;
}

print(go_outside(5, 'great'));
