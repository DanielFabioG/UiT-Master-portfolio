import numpy as np
from scipy.stats import norm



def plot_descrete_and_normal(p,x, dx, maxmin, plt):


	# Create the figure and axis
	fig, ax = plt.subplots()

	# Plot the bars with blue color and dark blue border
	bars = ax.bar(x, p, width=dx, align='center', 
		alpha=0.6, color='blue', edgecolor='darkblue',
		label="Bars (f(x) with width dx)")

	# Label each bar with its "mass" (height * dx) if dx is not too small
	if len(x) < 10:
		for bar, height in zip(bars, p):
			mass = height * dx
			ax.text(bar.get_x() + bar.get_width() / 2, 
				height, f'{mass:.3f}', ha='center', 
				va='bottom')
			
	# Plot the normal distribution curve in blue
	x_fine = np.linspace(-maxmin + 0.25*dx, maxmin - 0.25*dx, 1000)
	p_fine = norm.pdf(x_fine, 0, 1)
	ax.plot(x_fine, p_fine, label="Normal Distribution", 
		color='blue')
	
	# Add labels and title
	ax.set_xlabel('x')
	ax.set_title('Normal Distribution and '
		'corresponding discrete probabilities '
		' (dx = {})'.format(dx))
	ax.legend()
	return fig, ax
	
def plot(rho, x_gamble, p_gamble, u_func, x_func, plt):
	

	# Gamble expectations
	expected_utility = np.sum(p_gamble * u_func(x_gamble, rho))
	expected_value = np.sum(p_gamble * x_gamble)
	utility_of_expected_value = u_func(expected_value, rho)

	# Values for wealth and utility
	x_vals = np.linspace(x_gamble[0]-0.02, x_gamble[1]+0.02, 100)
	u_x = u_func(x_vals, rho)

	# Plotting the utility function using fig, ax objects
	fig, ax = plt.subplots(figsize=(10, 6))

	# Plotting the utility function
	ax.plot(x_vals, u_x, 
			label=('Utility Function:' 
					r'$u(x) = - \mathrm{e}(-\rho x)$'), 
			color='black')

	# Ploting the gamble outcomes and mean, by creating a function first


	plot_vertical(expected_value, ax, u_x)
	plot_vertical(x_gamble[0], ax, u_x)
	plot_vertical(x_gamble[1],ax, u_x)

	# Plot a line connecting the points where the utility function 
	# crosses 0.5 and 1.5

	ax.plot([x_gamble[0], x_gamble[1]], 
			[u_func(x_gamble[0], rho), u_func(x_gamble[1], rho)], 
			color='blue', linestyle='-', marker='o', 
			label='Any expected utility must lie on this line')

	# Plot the expected utility
	ax.axhline(y=expected_utility, color='gray', 
			linestyle='--', label='Expected Utility of Gamble $EU(X)$')
	ax.text(x_gamble[0]+0.02, expected_utility+0.005, '$EU(X)$', verticalalignment='top', fontsize=12)

	ax.text(expected_value, utility_of_expected_value, '$U(EX)$', verticalalignment='top', fontsize=12)
	ax.plot(expected_value, utility_of_expected_value, marker='o')

	# Risk premium - distance between expected utility and utility 
	# of certain outcome
	risk_premium = u_func(expected_value, rho) - expected_utility
	certainty_equivalence = x_func(expected_utility,   rho)
	ax.annotate('', xy=(expected_value, expected_utility), 
				xytext=(certainty_equivalence, expected_utility), 
				arrowprops=dict(facecolor='black', arrowstyle='<->'))

	risk_comp = np.round(expected_value-certainty_equivalence,2)
	ax.annotate(r'$\pi=$' +str(risk_comp), 
			xy=(0.5*(expected_value + certainty_equivalence), 
			expected_utility+0.002), fontsize=12)
	# Settings

	ax.set_xlabel('Wealth (x)')
	ax.set_ylabel(r'Utility U(x), $\rho='+str(rho)+r'$')
	ax.spines['right'].set_visible(False)
	ax.spines['top'].set_visible(False)
	ax.legend()
	  
	return fig, ax


def plot_vertical(x, ax, u_x):
	text = f"${x}$"
	ax.axvline(x=x, color='black', linestyle='--')
	ax.text(x, max(u_x)+0.005, text, 
				horizontalalignment='center', fontsize=12)
	


